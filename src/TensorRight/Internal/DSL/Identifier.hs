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
import Grisette
  ( AsMetadata (asMetadata, fromMetadata),
    PPrint (pformat),
    SExpr (Atom, List),
  )
import Language.Haskell.TH.Syntax (Lift)

data IdentifierKind = RClassKind | MapKind | TensorKind

data Identifier (kind :: IdentifierKind)
  = SimpleIdentifier T.Text
  | IndexedIdentifier T.Text Int
  deriving stock (Eq, Ord, Generic, Lift)
  deriving anyclass (Hashable, NFData)

instance AsMetadata (Identifier kind) where
  asMetadata (SimpleIdentifier name) = List [Atom "SimpleIdentifier", Atom name]
  asMetadata (IndexedIdentifier name i) =
    List [Atom "IndexedIdentifier", Atom name, Atom $ T.pack $ show i]
  fromMetadata (List [Atom "SimpleIdentifier", Atom name]) =
    Just $ SimpleIdentifier name
  fromMetadata (List [Atom "IndexedIdentifier", Atom name, Atom i]) =
    Just $ IndexedIdentifier name (read $ T.unpack i)
  fromMetadata _ = Nothing

instance IsString (Identifier kind) where
  fromString = SimpleIdentifier . T.pack

instance Show (Identifier kind) where
  show (SimpleIdentifier name) = T.unpack name
  show (IndexedIdentifier name i) = T.unpack name <> "@" <> show i

instance PPrint (Identifier kind) where
  pformat (SimpleIdentifier name) = pformat name
  pformat (IndexedIdentifier name i) = pformat name <> "@" <> pformat i

-- | An identifier for an RClass.
type RClassIdentifier = Identifier 'RClassKind

-- | An identifier for an aggregated-map.
type MapIdentifier = Identifier 'MapKind

-- | An identifier for a tensor.
type TensorIdentifier = Identifier 'TensorKind

nextIdentifier :: Identifier kind -> Identifier kind
nextIdentifier (SimpleIdentifier name) = IndexedIdentifier name 0
nextIdentifier (IndexedIdentifier name i) = IndexedIdentifier name (i + 1)

-- | A 'Label' represents a name for an aggregated-axis.
-- It is not needed if an RClass has only one aggregated-axis.
type Label = T.Text
