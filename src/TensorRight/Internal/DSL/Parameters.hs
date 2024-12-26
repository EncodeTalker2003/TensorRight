{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TensorRight.Internal.DSL.Parameters (IsParamMaps (..), ParamDesc (..)) where

import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Grisette (Default (Default), PPrint)
import TensorRight.Internal.DSL.Identifier (RClassIdentifier, MapIdentifier)
import TensorRight.Internal.DSL.Shape (RClassRef (ByRClass))
import TensorRight.Internal.DSL.Syntax (ArrowSyntax ((-->)))

class IsParamMaps m where
  toParamMaps :: m -> HM.HashMap RClassRef MapIdentifier

instance IsParamMaps (HM.HashMap RClassRef MapIdentifier) where
  toParamMaps = id

instance IsParamMaps [ParamDesc] where
  toParamMaps = foldr (HM.union . toParamMaps) HM.empty

data ParamDesc = ParamDesc RClassRef MapIdentifier
  deriving (Generic, Eq, Show)
  deriving (Hashable)
  deriving (PPrint) via (Default ParamDesc)

instance IsParamMaps ParamDesc where
  toParamMaps (ParamDesc rclass map) = HM.singleton rclass map

instance ArrowSyntax RClassIdentifier MapIdentifier ParamDesc where
  rclass --> map = ParamDesc (ByRClass rclass) map

instance ArrowSyntax RClassRef MapIdentifier ParamDesc where
  ref --> map = ParamDesc ref map
