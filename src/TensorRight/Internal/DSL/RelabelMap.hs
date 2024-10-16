{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TensorRight.Internal.DSL.RelabelMap (IsRelabelMap (..), RelabelMapDesc (..)) where

import qualified Data.HashMap.Lazy as HM
import TensorRight.Internal.DSL.Identifier (AdimIdentifier)
import TensorRight.Internal.DSL.Shape (AdimRef (ByAdim))
import TensorRight.Internal.DSL.Syntax (ArrowSyntax ((-->)))

class IsRelabelMap m where
  toRelabelMap :: m -> HM.HashMap AdimRef AdimRef

instance IsRelabelMap (HM.HashMap AdimRef AdimRef) where
  toRelabelMap = id

data RelabelMapDesc = RelabelMapDesc AdimRef AdimRef
  deriving (Eq, Show)

instance IsRelabelMap RelabelMapDesc where
  toRelabelMap (RelabelMapDesc adim1 adim2) = HM.singleton adim1 adim2

instance IsRelabelMap [RelabelMapDesc] where
  toRelabelMap = foldr (HM.union . toRelabelMap) HM.empty

instance ArrowSyntax AdimIdentifier AdimRef RelabelMapDesc where
  adim1 --> adim2 = RelabelMapDesc (ByAdim adim1) adim2

instance ArrowSyntax AdimIdentifier AdimIdentifier RelabelMapDesc where
  adim1 --> adim2 = RelabelMapDesc (ByAdim adim1) (ByAdim adim2)

instance ArrowSyntax AdimRef AdimRef RelabelMapDesc where
  ref --> adim = RelabelMapDesc ref adim
