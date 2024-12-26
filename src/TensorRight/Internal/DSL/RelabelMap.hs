{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TensorRight.Internal.DSL.RelabelMap (IsRelabelMap (..), RelabelMapDesc (..)) where

import qualified Data.HashMap.Lazy as HM
import TensorRight.Internal.DSL.Identifier (RClassIdentifier)
import TensorRight.Internal.DSL.Shape (RClassRef (ByRClass))
import TensorRight.Internal.DSL.Syntax (ArrowSyntax ((-->)))

class IsRelabelMap m where
  toRelabelMap :: m -> HM.HashMap RClassRef RClassRef

instance IsRelabelMap (HM.HashMap RClassRef RClassRef) where
  toRelabelMap = id

data RelabelMapDesc = RelabelMapDesc RClassRef RClassRef
  deriving (Eq, Show)

instance IsRelabelMap RelabelMapDesc where
  toRelabelMap (RelabelMapDesc rclass1 rclass2) = HM.singleton rclass1 rclass2

instance IsRelabelMap [RelabelMapDesc] where
  toRelabelMap = foldr (HM.union . toRelabelMap) HM.empty

instance ArrowSyntax RClassIdentifier RClassRef RelabelMapDesc where
  rclass1 --> rclass2 = RelabelMapDesc (ByRClass rclass1) rclass2

instance ArrowSyntax RClassIdentifier RClassIdentifier RelabelMapDesc where
  rclass1 --> rclass2 = RelabelMapDesc (ByRClass rclass1) (ByRClass rclass2)

instance ArrowSyntax RClassRef RClassRef RelabelMapDesc where
  ref --> rclass = RelabelMapDesc ref rclass
