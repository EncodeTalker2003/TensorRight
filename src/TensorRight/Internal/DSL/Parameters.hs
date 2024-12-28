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

-- | A type class for types that can be converted to a map from 'RClassRef' to
-- 'MapIdentifier'. These maps, called parameter maps, are used to specify the 
-- parameters or attributes of a tensor opertor.
class IsParamMaps m where
  toParamMaps :: m -> HM.HashMap RClassRef MapIdentifier

instance IsParamMaps (HM.HashMap RClassRef MapIdentifier) where
  toParamMaps = id

-- | TensorRight DSL uses a list of 'ParamDesc' to represent parameter maps
instance IsParamMaps [ParamDesc] where
  toParamMaps = foldr (HM.union . toParamMaps) HM.empty

-- | 'ParamDesc' describes a mapping in a parameter map, which consists of an
-- 'RClassRef' and a 'MapIdentifier'. A 'ParamDesc' can be created in the
-- following ways:
-- 
-- - Directly using the RClass for unlabelled mappings: @'RClassIdentifier' 'TensorRight.Internal.DSL.Syntax.-->' 'MapIdentifier'@
-- - Using a 'RClassRef' for labelled mappings: @'RClassRef' 'TensorRight.Internal.DSL.Syntax.-->' 'MapIdentifier'@
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
