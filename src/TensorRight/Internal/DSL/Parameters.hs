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
import TensorRight.Internal.DSL.Identifier (AdimIdentifier, MapIdentifier)
import TensorRight.Internal.DSL.Shape (AdimRef (ByAdim))
import TensorRight.Internal.DSL.Syntax (ArrowSyntax ((-->)))

class IsParamMaps m where
  toParamMaps :: m -> HM.HashMap AdimRef MapIdentifier

instance IsParamMaps (HM.HashMap AdimRef MapIdentifier) where
  toParamMaps = id

instance IsParamMaps [ParamDesc] where
  toParamMaps = foldr (HM.union . toParamMaps) HM.empty

data ParamDesc = ParamDesc AdimRef MapIdentifier
  deriving (Generic, Eq, Show)
  deriving (Hashable)
  deriving (PPrint) via (Default ParamDesc)

instance IsParamMaps ParamDesc where
  toParamMaps (ParamDesc adim map) = HM.singleton adim map

instance ArrowSyntax AdimIdentifier MapIdentifier ParamDesc where
  adim --> map = ParamDesc (ByAdim adim) map

instance ArrowSyntax AdimRef MapIdentifier ParamDesc where
  ref --> map = ParamDesc ref map
