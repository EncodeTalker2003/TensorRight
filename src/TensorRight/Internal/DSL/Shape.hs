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
    RClassRef (..),
    RClassRefSet,
    AbstractShape (..),
    toAbstractShape,
    TensorShapeLike (toTensorShape),
    TensorShapeDesc (..),
    abstractShapeAllRefs,
    removeRClass,
    getRClassByRClassRef,
    addRClassByRClassRef,
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
import TensorRight.Internal.DSL.Identifier (RClassIdentifier, Label, MapIdentifier)
import TensorRight.Internal.DSL.Syntax (ArrowSyntax ((-->)), AtSyntax ((@@)))
import TensorRight.Internal.Util.Error (Error, assert)
import TensorRight.Internal.Util.Pretty (encloseList, prettyWithConstructor)

-- | Reference to an rclass. An rclass may be labelled or not labelled.
data RClassRef
  = ByRClass RClassIdentifier
  | ByLabel Label
  deriving (Generic, Eq, Ord, Show)
  deriving (Hashable)
  deriving (PPrint) via (Default RClassRef)

type RClassRefSet = HS.HashSet RClassRef

data TensorShape = TensorShape
  { labelled :: HM.HashMap Label (RClassIdentifier, MapIdentifier),
    unlabelled :: HM.HashMap RClassIdentifier MapIdentifier
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
          [ prettyLabelled label rclass map
            | (label, (rclass, map)) <- HM.toList labelled
          ]
            ++ [ prettyUnlabelled rclass map
                 | (rclass, map) <- HM.toList unlabelled
               ]
      ]
    where
      prettyLabelled label rclass map =
        pformat rclass <> " -> " <> pformat map <> " @@ " <> pformat label
      prettyUnlabelled rclass map = pformat rclass <> " -> " <> pformat map

data PartialTensorShapeDesc
  = PartialTensorShapeDesc RClassIdentifier MapIdentifier

data TensorShapeDesc
  = UnlabelledDesc RClassIdentifier MapIdentifier
  | LabelledDesc Label RClassIdentifier MapIdentifier

instance ArrowSyntax RClassIdentifier MapIdentifier PartialTensorShapeDesc where
  (-->) = PartialTensorShapeDesc

instance ArrowSyntax RClassIdentifier MapIdentifier TensorShapeDesc where
  (-->) = UnlabelledDesc

instance AtSyntax PartialTensorShapeDesc Label TensorShapeDesc where
  PartialTensorShapeDesc rclass map @@ label = LabelledDesc label rclass map

getTensorShape' ::
  (MonadError Error m) => [TensorShapeDesc] -> m TensorShape
getTensorShape' [] = return $ TensorShape HM.empty HM.empty
getTensorShape' (UnlabelledDesc rclass map : rest) = do
  TensorShape labelled unlabelled <- getTensorShape' rest
  when (HM.member rclass unlabelled) $ throwError "Duplicate rclass without labels"
  return $
    TensorShape labelled (HM.insert rclass map unlabelled)
getTensorShape' (LabelledDesc label rclass map : rest) = do
  TensorShape labelled unlabelled <- getTensorShape' rest
  when (HM.member label labelled) $ throwError "Duplicate label"
  when (HM.member rclass unlabelled) $
    throwError "Labelled rclass already present as unlabelled"
  return $ TensorShape (HM.insert label (rclass, map) labelled) unlabelled

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
  { labelled :: HM.HashMap Label RClassIdentifier,
    unlabelled :: HS.HashSet RClassIdentifier
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
          [prettyLabelled label rclass | (label, rclass) <- HM.toList labelled]
            ++ [pformat rclass | rclass <- HS.toList unlabelled]
      ]
    where
      prettyLabelled label rclass = pformat rclass <> " @@ " <> pformat label

toAbstractShape :: TensorShape -> AbstractShape
toAbstractShape (TensorShape labelled unlabelled) =
  AbstractShape
    { labelled = HM.map fst labelled,
      unlabelled = HM.keysSet unlabelled
    }

abstractShapeAllRefs :: AbstractShape -> HS.HashSet RClassRef
abstractShapeAllRefs (AbstractShape labelled unlabelled) =
  HS.map ByRClass unlabelled `HS.union` HS.map ByLabel (HM.keysSet labelled)

removeRClass ::
  (MonadError T.Text m, TryMerge m) =>
  AbstractShape ->
  RClassRef ->
  m AbstractShape
removeRClass AbstractShape {..} (ByRClass rclass) = do
  assert "RClass not exist" $ HS.member rclass unlabelled
  return $ AbstractShape labelled (HS.delete rclass unlabelled)
removeRClass AbstractShape {..} (ByLabel label) = do
  assert "Label not exist" $ HM.member label labelled
  return $ AbstractShape (HM.delete label labelled) unlabelled

getRClassByRClassRef ::
  (MonadError T.Text m, TryMerge m) =>
  AbstractShape ->
  RClassRef ->
  m RClassIdentifier
getRClassByRClassRef AbstractShape {..} (ByRClass rclass) = do
  assert "RClass not exist" $ HS.member rclass unlabelled
  return rclass
getRClassByRClassRef AbstractShape {..} (ByLabel label) =
  case HM.lookup label labelled of
    Nothing -> throwError "Label not exist"
    Just rclass -> return rclass

addRClassByRClassRef ::
  (MonadError T.Text m, TryMerge m) =>
  AbstractShape ->
  RClassRef ->
  RClassIdentifier ->
  m AbstractShape
addRClassByRClassRef AbstractShape {..} (ByRClass rclass) rclass' = do
  assert "RClass already exist" $ not $ HS.member rclass unlabelled
  assert "If adding by rclass itself, then rclass must be the same" $ rclass == rclass'
  return $ AbstractShape labelled (HS.insert rclass' unlabelled)
addRClassByRClassRef AbstractShape {..} (ByLabel label) rclass = do
  assert "Label already exist" $ not $ HM.member label labelled
  return $ AbstractShape (HM.insert label rclass labelled) unlabelled

concatAbstractShape ::
  (MonadError T.Text m, TryMerge m) =>
  AbstractShape ->
  AbstractShape ->
  m AbstractShape
concatAbstractShape
  (AbstractShape l1 u1)
  (AbstractShape l2 u2) = do
    assert "Labelled rclass overlap" $ HM.null $ HM.intersection l1 l2
    assert "Unlabelled rclass overlap" $ HS.null $ HS.intersection u1 u2
    let newUnlabelledRClasses = u1 `HS.union` u2
    let newLabelled = l1 `HM.union` l2
    let newLabelledRClasses = HS.fromList $ HM.elems newLabelled
    assert "Labelled rclass overlap with unlabelled" $
      HS.null $
        HS.intersection newUnlabelledRClasses newLabelledRClasses
    let newAbstractShape =
          AbstractShape
            (l1 `HM.union` l2)
            (u1 `HS.union` u2)
    return newAbstractShape

restrictAbstractShape ::
  (MonadError T.Text m, TryMerge m) =>
  AbstractShape ->
  RClassRefSet ->
  m AbstractShape
restrictAbstractShape AbstractShape {..} rclasses = do
  let newLabelled =
        HM.filterWithKey (\k _ -> k `HS.member` byLabel) labelled
  let newUnlabelled = HS.intersection unlabelled byRClass
  assert "restrictAbstractShape: some label does not exist" $
    HS.size byLabel == HM.size newLabelled
  assert "restrictAbstractShape: some rclass does not exist" $
    HS.size byRClass == HS.size newUnlabelled
  return $ AbstractShape newLabelled newUnlabelled
  where
    byLabel' [] = []
    byLabel' (ByLabel label : as) = label : byLabel' as
    byLabel' (_ : as) = byLabel' as
    byLabel = HS.fromList $ byLabel' $ HS.toList rclasses

    byRClass' [] = []
    byRClass' (ByRClass rclass : as) = rclass : byRClass' as
    byRClass' (_ : as) = byRClass' as
    byRClass = HS.fromList $ byRClass' $ HS.toList rclasses
