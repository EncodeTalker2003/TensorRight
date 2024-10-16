{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TensorRight.Internal.DSL.Shape
  ( TensorShape (..),
    AdimRef (..),
    AdimRefSet,
    AbstractShape (..),
    toAbstractShape,
    TensorShapeLike (toTensorShape),
    TensorShapeDesc (..),
    abstractShapeAllRefs,
    removeAdim,
    getAdimByAdimRef,
    addAdimByAdimRef,
    concatAbstractShape,
    restrictAbstractShape,
  )
where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (hashWithSalt))
import Data.List (sortBy)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (Default (Default), PPrint (pformat, pformatPrec), TryMerge)
import TensorRight.Internal.DSL.Identifier (AdimIdentifier, Label, MapIdentifier)
import TensorRight.Internal.DSL.Syntax (ArrowSyntax ((-->)), AtSyntax ((@@)))
import TensorRight.Internal.Util.Error (Error, assert)
import TensorRight.Internal.Util.Pretty (encloseList, prettyWithConstructor)

-- | Reference to an adim. An adim may be labelled or not labelled.
data AdimRef
  = ByAdim AdimIdentifier
  | ByLabel Label
  deriving (Generic, Eq, Ord, Show)
  deriving (Hashable)
  deriving (PPrint) via (Default AdimRef)

type AdimRefSet = HS.HashSet AdimRef

data TensorShape = TensorShape
  { labelled :: HM.HashMap Label (AdimIdentifier, MapIdentifier),
    unlabelled :: HM.HashMap AdimIdentifier MapIdentifier
  }
  deriving (Show, Generic)

instance Eq TensorShape where
  (TensorShape l1 u1) == (TensorShape l2 u2) = l1 == l2 && u1 == u2

instance Hashable TensorShape where
  hashWithSalt salt (TensorShape labelled unlabelled) =
    salt
      `hashWithSalt` HM.toList labelled
      `hashWithSalt` HM.toList unlabelled

instance PPrint TensorShape where
  pformatPrec n (TensorShape labelled unlabelled) =
    prettyWithConstructor
      n
      "TensorShape"
      [ encloseList "{" "}" "," $
          [ prettyLabelled label adim map
            | (label, (adim, map)) <- HM.toList labelled
          ]
            ++ [ prettyUnlabelled adim map
                 | (adim, map) <- HM.toList unlabelled
               ]
      ]
    where
      prettyLabelled label adim map =
        pformat adim <> " -> " <> pformat map <> " @@ " <> pformat label
      prettyUnlabelled adim map = pformat adim <> " -> " <> pformat map

data PartialTensorShapeDesc
  = PartialTensorShapeDesc AdimIdentifier MapIdentifier

data TensorShapeDesc
  = UnlabelledDesc AdimIdentifier MapIdentifier
  | LabelledDesc Label AdimIdentifier MapIdentifier

instance ArrowSyntax AdimIdentifier MapIdentifier PartialTensorShapeDesc where
  (-->) = PartialTensorShapeDesc

instance ArrowSyntax AdimIdentifier MapIdentifier TensorShapeDesc where
  (-->) = UnlabelledDesc

instance AtSyntax PartialTensorShapeDesc Label TensorShapeDesc where
  PartialTensorShapeDesc adim map @@ label = LabelledDesc label adim map

getTensorShape' ::
  (MonadError Error m) => [TensorShapeDesc] -> m TensorShape
getTensorShape' [] = return $ TensorShape HM.empty HM.empty
getTensorShape' (UnlabelledDesc adim map : rest) = do
  TensorShape labelled unlabelled <- getTensorShape' rest
  when (HM.member adim unlabelled) $ throwError "Duplicate adim without labels"
  return $
    TensorShape labelled (HM.insert adim map unlabelled)
getTensorShape' (LabelledDesc label adim map : rest) = do
  TensorShape labelled unlabelled <- getTensorShape' rest
  when (HM.member label labelled) $ throwError "Duplicate label"
  when (HM.member adim unlabelled) $
    throwError "Labelled adim already present as unlabelled"
  return $ TensorShape (HM.insert label (adim, map) labelled) unlabelled

getTensorShape ::
  (MonadError Error m) => [TensorShapeDesc] -> m TensorShape
getTensorShape descs =
  getTensorShape' $
    sortBy
      ( \a b ->
          case (a, b) of
            (LabelledDesc {}, UnlabelledDesc {}) -> LT
            (UnlabelledDesc {}, LabelledDesc {}) -> GT
            _ -> EQ
      )
      descs

class TensorShapeLike a where
  toTensorShape :: (MonadError Error m) => a -> m TensorShape

instance TensorShapeLike TensorShape where
  toTensorShape = return

instance TensorShapeLike [TensorShapeDesc] where
  toTensorShape = getTensorShape

data AbstractShape = AbstractShape
  { labelled :: HM.HashMap Label AdimIdentifier,
    unlabelled :: HS.HashSet AdimIdentifier
  }
  deriving (Show)

instance Eq AbstractShape where
  (AbstractShape l1 u1) == (AbstractShape l2 u2) = l1 == l2 && u1 == u2

instance PPrint AbstractShape where
  pformatPrec n (AbstractShape labelled unlabelled) =
    prettyWithConstructor
      n
      "AbstractShape"
      [ encloseList "{" "}" "," $
          [prettyLabelled label adim | (label, adim) <- HM.toList labelled]
            ++ [pformat adim | adim <- HS.toList unlabelled]
      ]
    where
      prettyLabelled label adim = pformat adim <> " @@ " <> pformat label

toAbstractShape :: TensorShape -> AbstractShape
toAbstractShape (TensorShape labelled unlabelled) =
  AbstractShape
    { labelled = HM.map fst labelled,
      unlabelled = HM.keysSet unlabelled
    }

abstractShapeAllRefs :: AbstractShape -> HS.HashSet AdimRef
abstractShapeAllRefs (AbstractShape labelled unlabelled) =
  HS.map ByAdim unlabelled `HS.union` HS.map ByLabel (HM.keysSet labelled)

removeAdim ::
  (MonadError T.Text m, TryMerge m) =>
  AbstractShape ->
  AdimRef ->
  m AbstractShape
removeAdim AbstractShape {..} (ByAdim adim) = do
  assert "Adim not exist" $ HS.member adim unlabelled
  return $ AbstractShape labelled (HS.delete adim unlabelled)
removeAdim AbstractShape {..} (ByLabel label) = do
  assert "Label not exist" $ HM.member label labelled
  return $ AbstractShape (HM.delete label labelled) unlabelled

getAdimByAdimRef ::
  (MonadError T.Text m, TryMerge m) =>
  AbstractShape ->
  AdimRef ->
  m AdimIdentifier
getAdimByAdimRef AbstractShape {..} (ByAdim adim) = do
  assert "Adim not exist" $ HS.member adim unlabelled
  return adim
getAdimByAdimRef AbstractShape {..} (ByLabel label) =
  case HM.lookup label labelled of
    Nothing -> throwError "Label not exist"
    Just adim -> return adim

addAdimByAdimRef ::
  (MonadError T.Text m, TryMerge m) =>
  AbstractShape ->
  AdimRef ->
  AdimIdentifier ->
  m AbstractShape
addAdimByAdimRef AbstractShape {..} (ByAdim adim) adim' = do
  assert "Adim already exist" $ not $ HS.member adim unlabelled
  assert "If adding by adim itself, then adim must be the same" $ adim == adim'
  return $ AbstractShape labelled (HS.insert adim' unlabelled)
addAdimByAdimRef AbstractShape {..} (ByLabel label) adim = do
  assert "Label already exist" $ not $ HM.member label labelled
  return $ AbstractShape (HM.insert label adim labelled) unlabelled

concatAbstractShape ::
  (MonadError T.Text m, TryMerge m) =>
  AbstractShape ->
  AbstractShape ->
  m AbstractShape
concatAbstractShape
  (AbstractShape l1 u1)
  (AbstractShape l2 u2) = do
    assert "Labelled adim overlap" $ HM.null $ HM.intersection l1 l2
    assert "Unlabelled adim overlap" $ HS.null $ HS.intersection u1 u2
    let newUnlabelledAdims = u1 `HS.union` u2
    let newLabelled = l1 `HM.union` l2
    let newLabelledAdims = HS.fromList $ HM.elems newLabelled
    assert "Labelled adim overlap with unlabelled" $
      HS.null $
        HS.intersection newUnlabelledAdims newLabelledAdims
    let newAbstractShape =
          AbstractShape
            (l1 `HM.union` l2)
            (u1 `HS.union` u2)
    return newAbstractShape

restrictAbstractShape ::
  (MonadError T.Text m, TryMerge m) =>
  AbstractShape ->
  AdimRefSet ->
  m AbstractShape
restrictAbstractShape AbstractShape {..} adims = do
  let newLabelled =
        HM.filterWithKey (\k _ -> k `HS.member` byLabel) labelled
  let newUnlabelled = HS.intersection unlabelled byAdim
  assert "restrictAbstractShape: some label does not exist" $
    HS.size byLabel == HM.size newLabelled
  assert "restrictAbstractShape: some adim does not exist" $
    HS.size byAdim == HS.size newUnlabelled
  return $ AbstractShape newLabelled newUnlabelled
  where
    byLabel' [] = []
    byLabel' (ByLabel label : as) = label : byLabel' as
    byLabel' (_ : as) = byLabel' as
    byLabel = HS.fromList $ byLabel' $ HS.toList adims

    byAdim' [] = []
    byAdim' (ByAdim adim : as) = adim : byAdim' as
    byAdim' (_ : as) = byAdim' as
    byAdim = HS.fromList $ byAdim' $ HS.toList adims
