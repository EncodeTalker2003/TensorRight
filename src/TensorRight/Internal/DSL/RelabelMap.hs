{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TensorRight.Internal.DSL.RelabelMap (IsRelabelMap (..), RelabelMapDesc (..)) where

import qualified Data.HashMap.Lazy as HM
import TensorRight.Internal.DSL.Identifier (RClassIdentifier)
import TensorRight.Internal.DSL.Shape (RClassRef (ByRClass))
import TensorRight.Internal.DSL.Syntax (ArrowSyntax ((-->)))

-- | A type class for types that can be converted to a map from 'RClassRef' to
-- 'RClassRef'. These maps, called relabel maps, are used to specify the
-- axis relabling mapping for the `relabel` operator.
class IsRelabelMap m where
  toRelabelMap :: m -> HM.HashMap RClassRef RClassRef

instance IsRelabelMap (HM.HashMap RClassRef RClassRef) where
  toRelabelMap = id

-- | 'RelabelMapDesc' describes a single relabling in a relabel map, which
-- consists of an 'RClassRef' and another 'RClassRef'. A 'RelabelMapDesc' can be
-- created using the syntax @'RClassRef' 'TensorRight.Internal.DSL.Syntax.-->' 'RClassRef'@
data RelabelMapDesc = RelabelMapDesc RClassRef RClassRef
  deriving (Eq, Show)

instance IsRelabelMap RelabelMapDesc where
  toRelabelMap (RelabelMapDesc rclass1 rclass2) = HM.singleton rclass1 rclass2

-- | TensorRight DSL uses a list of 'RelabelMapDesc' to represent relabel maps
instance IsRelabelMap [RelabelMapDesc] where
  toRelabelMap = foldr (HM.union . toRelabelMap) HM.empty

instance ArrowSyntax RClassIdentifier RClassRef RelabelMapDesc where
  rclass1 --> rclass2 = RelabelMapDesc (ByRClass rclass1) rclass2

instance ArrowSyntax RClassIdentifier RClassIdentifier RelabelMapDesc where
  rclass1 --> rclass2 = RelabelMapDesc (ByRClass rclass1) (ByRClass rclass2)

instance ArrowSyntax RClassRef RClassRef RelabelMapDesc where
  ref --> rclass = RelabelMapDesc ref rclass
