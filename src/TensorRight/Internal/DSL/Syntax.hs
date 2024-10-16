{-# LANGUAGE FunctionalDependencies #-}

module TensorRight.Internal.DSL.Syntax (ArrowSyntax (..), AtSyntax (..)) where

-- | Syntax for defining a mapping.
class ArrowSyntax a b c where
  (-->) :: a -> b -> c
  infix 8 -->

-- | Syntax for labelling an adim.
class AtSyntax a b c | c -> a b where
  (@@) :: a -> b -> c
  infix 7 @@
