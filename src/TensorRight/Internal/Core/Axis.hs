{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TensorRight.Internal.Core.Axis
  ( Axis (..),
    Indices,
    Sizes,
    AxisMapLike (..),
    Axes,
    unionAxisMap,
    allAxes,
    lookupAxis,
    getAxis,
    removeAxes,
    restrictAxes,
    mapAxisMap,
    mapAxisMapWithAxisKey,
    zipFoldAxisMap,
    zipAxisMap,
    zipAxisMap3,
    zipAxisMapM,
    zipAxisMapM3,
    addAxisMap,
    mulAxisMap,
    subAxisMap,
    foldAxisMap,
    castAxisMap,
    intersectionAxisMap,
    sameAxisMap,
    safeDivAxisMap,
    safeModAxisMap,
  )
where

import Control.Exception (ArithException)
import Data.Foldable (Foldable (toList))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.List (sortOn)
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Grisette
  ( Default (Default),
    EvalSym (evalSym),
    ExtractSym (extractSymMaybe),
    LogicalOp ((.&&)),
    Mergeable (rootStrategy),
    MergingStrategy (SimpleStrategy, SortedStrategy),
    MonadTryMerge,
    PPrint (pformat),
    SafeDiv (safeDiv, safeMod),
    SimpleMergeable (mrgIte),
    Solvable (con),
    SymBool,
    SymEq ((.==)),
    SymInteger,
  )
import Grisette.Lib.Control.Monad.Except (mrgModifyError)
import TensorRight.Internal.Util.Error (ErrorEnv)

data Axis
  = Axis {_axis :: T.Text}
  | LabelledAxis {_label :: T.Text, _axis :: T.Text}
  deriving (Generic, Ord, Eq)
  deriving anyclass (Hashable)
  deriving (Mergeable, SymEq, EvalSym) via (Default Axis)

instance PPrint Axis where
  pformat (Axis a) = pformat a
  pformat (LabelledAxis l a) = pformat l <> "@@" <> pformat a

instance Show Axis where
  show (Axis a) = T.unpack a
  show (LabelledAxis l a) = T.unpack l ++ "@@" ++ T.unpack a

newtype UnifiedMap = UnifiedMap
  { unUnifiedMap :: HM.HashMap Axis SymInteger
  }
  deriving newtype (Show, Eq)
  deriving newtype (Semigroup, Monoid)

instance PPrint UnifiedMap where
  pformat = pformat . asHashMap

instance SymEq UnifiedMap where
  UnifiedMap l .== UnifiedMap r =
    sortOn fst (HM.toList l) .== sortOn fst (HM.toList r)

instance EvalSym UnifiedMap where
  evalSym b m = fromKVPairs . evalSym b m . HM.toList . asHashMap

instance ExtractSym UnifiedMap where
  extractSymMaybe (UnifiedMap m) = extractSymMaybe $ snd <$> HM.toList m

instance Mergeable UnifiedMap where
  rootStrategy =
    SortedStrategy (HM.keys . unUnifiedMap) $
      const $
        SimpleStrategy $ \c (UnifiedMap d1) (UnifiedMap d2) ->
          UnifiedMap $ HM.unionWith (mrgIte c) d1 d2

instance AxisMapLike UnifiedMap where
  fromHashMap = UnifiedMap
  asHashMap = unUnifiedMap

allAxes :: (AxisMapLike m) => m -> Axes
allAxes = HS.fromList . HM.keys . asHashMap

lookupAxis :: (AxisMapLike m) => Axis -> m -> Maybe SymInteger
lookupAxis a = HM.lookup a . asHashMap

getAxis :: (AxisMapLike m) => Axis -> m -> SymInteger
getAxis a = (HM.! a) . asHashMap

removeAxes :: (AxisMapLike m) => Axes -> m -> m
removeAxes axes =
  fromHashMap . HM.filterWithKey (\k _ -> not $ k `HS.member` axes) . asHashMap

restrictAxes :: (AxisMapLike m) => Axes -> m -> m
restrictAxes axes =
  fromHashMap . HM.filterWithKey (\k _ -> k `HS.member` axes) . asHashMap

castAxisMap :: (AxisMapLike m1, AxisMapLike m2) => m1 -> m2
castAxisMap = fromHashMap . asHashMap

unionAxisMap :: (AxisMapLike m) => m -> m -> m
unionAxisMap l r = fromHashMap $ HM.union (asHashMap l) (asHashMap r)

intersectionAxisMap :: (AxisMapLike m) => m -> m -> m
intersectionAxisMap l r =
  fromHashMap $ HM.intersection (asHashMap l) (asHashMap r)

mapAxisMap :: (AxisMapLike m) => (SymInteger -> SymInteger) -> m -> m
mapAxisMap f m = fromHashMap $ HM.map f (asHashMap m)

mapAxisMapWithAxisKey ::
  (AxisMapLike m) => (Axis -> SymInteger -> SymInteger) -> m -> m
mapAxisMapWithAxisKey f m = fromHashMap $ HM.mapWithKey f (asHashMap m)

zipAxisMap ::
  (HasCallStack, AxisMapLike m) =>
  (SymInteger -> SymInteger -> SymInteger) ->
  m ->
  m ->
  m
zipAxisMap f l r
  | allAxes l == allAxes r =
      fromHashMap $ HM.unionWith f (asHashMap l) (asHashMap r)
  | otherwise = error "Cannot zip maps with different axes"

foldAxisMap ::
  (AxisMapLike m) =>
  (SymInteger -> a) ->
  a ->
  (a -> a -> a) ->
  m ->
  a
foldAxisMap f initial g m =
  HM.foldl' (\acc vl -> g acc (f vl)) initial (asHashMap m)

zipFoldAxisMap ::
  (HasCallStack, AxisMapLike m) =>
  (SymInteger -> SymInteger -> a) ->
  a ->
  (a -> a -> a) ->
  m ->
  m ->
  a
zipFoldAxisMap f initial g l r
  | allAxes l == allAxes r =
      HM.foldlWithKey'
        (\acc k vl -> g acc (f vl (getAxis k r)))
        initial
        (asHashMap l)
  | otherwise = error "Cannot zip maps with different axes"

zipAxisMap3 ::
  (HasCallStack, AxisMapLike m) =>
  (SymInteger -> SymInteger -> SymInteger -> SymInteger) ->
  m ->
  m ->
  m ->
  m
zipAxisMap3 f l r s
  | allAxes l == allAxes r && allAxes l == allAxes s =
      fromHashMap $
        HM.mapWithKey (\k vl -> f vl (getAxis k r) (getAxis k s)) $
          asHashMap l
  | otherwise = error "Cannot zip maps with different axes"

zipAxisMapM ::
  (HasCallStack, AxisMapLike am, MonadTryMerge m) =>
  (SymInteger -> SymInteger -> m SymInteger) ->
  am ->
  am ->
  m am
zipAxisMapM f l r
  | allAxes l == allAxes r =
      fromHashMap
        <$> HM.traverseWithKey (\k vl -> f vl (getAxis k r)) (asHashMap l)
  | otherwise = error "Cannot zip maps with different axes"

zipAxisMapM3 ::
  ( HasCallStack,
    AxisMapLike am,
    MonadTryMerge m
  ) =>
  (SymInteger -> SymInteger -> SymInteger -> m SymInteger) ->
  am ->
  am ->
  am ->
  m am
zipAxisMapM3 f l r s
  | allAxes l == allAxes r && allAxes l == allAxes s =
      fromHashMap
        <$> HM.traverseWithKey
          (\k vl -> f vl (getAxis k r) (getAxis k s))
          (asHashMap l)
  | otherwise = error "Cannot zip maps with different axes"

newtype Indices = Indices UnifiedMap
  deriving newtype
    ( Show,
      Eq,
      Mergeable,
      AxisMapLike,
      Semigroup,
      Monoid,
      SymEq,
      EvalSym,
      ExtractSym,
      PPrint
    )

newtype Sizes = Sizes UnifiedMap
  deriving newtype
    ( Show,
      Eq,
      Mergeable,
      AxisMapLike,
      Semigroup,
      Monoid,
      SymEq,
      ExtractSym,
      EvalSym,
      PPrint
    )

class (Monoid m) => AxisMapLike m where
  fromHashMap :: HM.HashMap Axis SymInteger -> m
  fromKVPairs :: (Foldable t) => t (Axis, SymInteger) -> m
  fromKVPairs = fromHashMap . HM.fromList . toList
  asHashMap :: m -> HM.HashMap Axis SymInteger

type Axes = HS.HashSet Axis

addAxisMap :: (HasCallStack, AxisMapLike m) => m -> m -> m
addAxisMap = zipAxisMap (+)

subAxisMap :: (HasCallStack, AxisMapLike m) => m -> m -> m
subAxisMap = zipAxisMap (-)

mulAxisMap :: (HasCallStack, AxisMapLike m) => m -> m -> m
mulAxisMap = zipAxisMap (*)

sameAxisMap :: (HasCallStack, AxisMapLike m) => m -> m -> SymBool
sameAxisMap l r =
  foldl
    (\acc dimName -> acc .&& getAxis dimName l .== getAxis dimName r)
    (con (allAxes l == allAxes r))
    (allAxes l)

safeDivAxisMap ::
  (HasCallStack, AxisMapLike m, Mergeable m) =>
  m ->
  m ->
  ErrorEnv m
safeDivAxisMap =
  zipAxisMapM
    ( \l r ->
        mrgModifyError (\(_ :: ArithException) -> "Division by zero") $
          safeDiv l r
    )

safeModAxisMap ::
  (HasCallStack, AxisMapLike m, Mergeable m) =>
  m ->
  m ->
  ErrorEnv m
safeModAxisMap =
  zipAxisMapM
    ( \l r ->
        mrgModifyError (\(_ :: ArithException) -> "Division by zero") $
          safeMod l r
    )
