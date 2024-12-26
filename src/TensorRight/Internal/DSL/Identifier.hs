{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module TensorRight.Internal.DSL.Identifier
  ( IdentifierKind (..),
    Identifier (..),
    RClassIdentifier,
    MapIdentifier,
    Label,
    TensorIdentifier,
    nextIdentifier,
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (PPrint (pformat))
import Language.Haskell.TH.Syntax (Lift)

data IdentifierKind = RClassKind | MapKind | TensorKind

data Identifier (kind :: IdentifierKind)
  = SimpleIdentifier T.Text
  | IndexedIdentifier T.Text Int
  deriving stock (Eq, Ord, Generic, Lift)
  deriving anyclass (Hashable, NFData)

instance IsString (Identifier kind) where
  fromString = SimpleIdentifier . T.pack

instance Show (Identifier kind) where
  show (SimpleIdentifier name) = T.unpack name
  show (IndexedIdentifier name i) = T.unpack name <> "@" <> show i

instance PPrint (Identifier kind) where
  pformat (SimpleIdentifier name) = pformat name
  pformat (IndexedIdentifier name i) = pformat name <> "@" <> pformat i

type RClassIdentifier = Identifier 'RClassKind

type MapIdentifier = Identifier 'MapKind

type TensorIdentifier = Identifier 'TensorKind

nextIdentifier :: Identifier kind -> Identifier kind
nextIdentifier (SimpleIdentifier name) = IndexedIdentifier name 0
nextIdentifier (IndexedIdentifier name i) = IndexedIdentifier name (i + 1)

type Label = T.Text
