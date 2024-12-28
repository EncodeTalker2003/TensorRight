{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TensorRight.Internal.Core.Tensor.TensorInt
  ( TensorNum (..),
    TensorDivMod (..),
    TensorExp (..),
    TensorInt,
    TensorReal,
    nonInf,
    posInf,
    negInf,
    tensorValSymMax,
    tensorValSymMin,
    tensorValEq,
    tensorValNe,
    tensorValLt,
    tensorValGt,
    tensorValLe,
    tensorValGe,
    IsTensorNum,
  )
where

import Data.Hashable (Hashable)
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    DivOr (divOr, modOr, remOr),
    EvalSym,
    FdivOr (fdivOr),
    ITEOp (symIte),
    LogicalOp (symNot, symXor, (.&&), (.||)),
    Mergeable,
    PPrint (pformat),
    SimpleMergeable (mrgIte),
    Solvable (con),
    SymAlgReal,
    SymBool,
    SymEq ((.==)),
    SymInteger,
    SymOrd ((.<), (.>)),
    Union,
    derive,
    liftUnion,
    mrgFmap,
    mrgIf,
    mrgReturn,
    symMax,
    symMin,
    pattern Con,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import TensorRight.Internal.Util.Error (ErrorEnv)

data TensorNumBase a
  = NonInf a
  | Inf SymBool -- ^ Positive or negative infinity
  | Unknown

derive
  ''TensorNumBase
  [ ''Generic,
    ''Show,
    ''Mergeable,
    ''Eq,
    ''Hashable,
    ''EvalSym
  ]

-- | A tensor of numerical values that may contain positive or negative infinity.
newtype TensorNum a = TensorNum (Union (TensorNumBase a))

type TensorInt = TensorNum SymInteger

type TensorReal = TensorNum SymAlgReal

instance (PPrint a) => PPrint (TensorNumBase a) where
  pformat (NonInf u) = pformat u
  pformat (Inf u) = case u of
    Con v -> if v then "inf" else "-inf"
    _ -> "?inf"
  pformat Unknown = "unk"

instance (SymEq a) => SymEq (TensorNumBase a) where
  NonInf a .== NonInf b = a .== b
  Inf a .== Inf b = a .== b
  Unknown .== Unknown = con False
  _ .== _ = con False

derive
  ''TensorNum
  [ ''Generic,
    ''Show,
    ''SymEq,
    ''Eq,
    ''Hashable,
    ''EvalSym,
    ''PPrint
  ]

deriving newtype instance (Mergeable a) => Mergeable (TensorNum a)

deriving newtype instance (SimpleMergeable a) => SimpleMergeable (TensorNum a)

instance (Mergeable a) => ITEOp (TensorNum a) where
  symIte c (TensorNum l) (TensorNum r) = TensorNum $ mrgIte c l r

-- | Wrap a symbolic integer into a tensor numerical value.
nonInf :: (Mergeable a) => a -> TensorNum a
nonInf = TensorNum . mrgReturn . NonInf

-- | Positive infinity.
posInf :: (Mergeable a) => TensorNum a
posInf = TensorNum $ mrgReturn $ Inf (con True)

-- | Negative infinity.
negInf :: (Mergeable a) => TensorNum a
negInf = TensorNum $ mrgReturn $ Inf (con False)

instance (Mergeable a, Num a, SymEq a, SymOrd a, ITEOp a) => Num (TensorNum a) where
  (TensorNum l) + (TensorNum r) = TensorNum $ do
    l1 <- l
    r1 <- r
    case (l1, r1) of
      (NonInf lv, NonInf rv) -> mrgReturn $ NonInf $ lv + rv
      (Inf lv, Inf rv) ->
        mrgIf (lv .== rv) (mrgReturn $ Inf lv) (mrgReturn Unknown)
      (NonInf _, Inf rv) -> mrgReturn $ Inf rv
      (Inf lv, NonInf _) -> mrgReturn $ Inf lv
      _ -> mrgReturn Unknown
  (TensorNum l) * (TensorNum r) = TensorNum $ do
    l1 <- l
    r1 <- r
    case (l1, r1) of
      (NonInf lv, NonInf rv) -> mrgReturn $ NonInf $ lv * rv
      (Inf lv, Inf rv) ->
        mrgIf
          (symXor lv rv)
          (mrgReturn $ Inf (con False))
          (mrgReturn $ Inf (con True))
      (NonInf lv, Inf rv) ->
        mrgIf
          (lv .== 0)
          (mrgReturn Unknown)
          (mrgIf (lv .> 0) (mrgReturn $ Inf rv) (mrgReturn $ Inf (symNot rv)))
      (Inf lv, NonInf rv) ->
        mrgIf
          (rv .== 0)
          (mrgReturn Unknown)
          (mrgIf (rv .> 0) (mrgReturn $ Inf lv) (mrgReturn $ Inf (symNot lv)))
      _ -> mrgReturn Unknown
  abs (TensorNum l) = TensorNum $ do
    l1 <- l
    case l1 of
      NonInf lv -> mrgReturn $ NonInf $ abs lv
      Inf _ -> mrgReturn $ Inf (con True)
      Unknown -> mrgReturn Unknown
  signum (TensorNum l) = TensorNum $ do
    l1 <- l
    case l1 of
      NonInf lv -> mrgReturn $ NonInf $ signum lv
      Inf lv -> mrgReturn $ NonInf $ symIte lv 1 (-1)
      Unknown -> mrgReturn Unknown
  fromInteger i = TensorNum $ mrgReturn $ NonInf $ fromInteger i
  negate (TensorNum l) = TensorNum $ do
    l1 <- l
    case l1 of
      NonInf lv -> mrgReturn $ NonInf $ negate lv
      Inf lv -> mrgReturn $ Inf lv
      Unknown -> mrgReturn Unknown

-- | Computes the maximum of two tensor numerical values.
tensorValSymMax ::
  (Mergeable a, SymOrd a, ITEOp a) => TensorNum a -> TensorNum a -> TensorNum a
tensorValSymMax (TensorNum l) (TensorNum r) = TensorNum $ do
  l1 <- l
  r1 <- r
  case (l1, r1) of
    (NonInf lv, NonInf rv) -> mrgReturn $ NonInf $ symMax lv rv
    (Inf lv, Inf rv) -> mrgReturn $ Inf $ lv .|| rv
    (NonInf lv, Inf rv) -> mrgIf rv (mrgReturn $ Inf rv) (mrgReturn $ NonInf lv)
    (Inf lv, NonInf rv) -> mrgIf lv (mrgReturn $ Inf lv) (mrgReturn $ NonInf rv)
    _ -> mrgReturn Unknown

-- | Computes the minimum of two tensor numerical values.
tensorValSymMin ::
  (Mergeable a, SymOrd a, ITEOp a) => TensorNum a -> TensorNum a -> TensorNum a
tensorValSymMin (TensorNum l) (TensorNum r) = TensorNum $ do
  l1 <- l
  r1 <- r
  case (l1, r1) of
    (NonInf lv, NonInf rv) -> mrgReturn $ NonInf $ symMin lv rv
    (Inf lv, Inf rv) -> mrgReturn $ Inf $ lv .&& rv
    (NonInf lv, Inf rv) -> mrgIf rv (mrgReturn $ NonInf lv) (mrgReturn $ Inf rv)
    (Inf lv, NonInf rv) -> mrgIf lv (mrgReturn $ NonInf rv) (mrgReturn $ Inf lv)
    _ -> mrgReturn Unknown

-- | Checks if two tensor numerical values are equal.
tensorValEq :: (Mergeable a, SymEq a) => TensorNum a -> TensorNum a -> ErrorEnv SymBool
tensorValEq (TensorNum l) (TensorNum r) = do
  l1 <- liftUnion l
  r1 <- liftUnion r
  case (l1, r1) of
    (NonInf lv, NonInf rv) -> return $ lv .== rv
    (Inf lv, Inf rv) -> return $ lv .== rv
    (Unknown, _) ->
      mrgThrowError "tensorValEq: Unsupported reasoning: comparing unknown values"
    (_, Unknown) ->
      mrgThrowError "tensorValEq: Unsupported reasoning: comparing unknown values"
    _ -> return $ con False

-- | Checks if two tensor numerical values are not equal.
tensorValNe :: (Mergeable a, SymEq a) => TensorNum a -> TensorNum a -> ErrorEnv SymBool
tensorValNe l r = mrgFmap symNot $ tensorValEq l r

-- | Checks if one tensor numerical value is less than another.
tensorValLt :: (Mergeable a, SymOrd a) => TensorNum a -> TensorNum a -> ErrorEnv SymBool
tensorValLt (TensorNum l) (TensorNum r) = do
  l1 <- liftUnion l
  r1 <- liftUnion r
  case (l1, r1) of
    (NonInf lv, NonInf rv) -> mrgReturn $ lv .< rv
    (Inf lv, Inf rv) -> mrgReturn $ lv .< rv
    (Unknown, _) ->
      mrgThrowError "tensorValLt: Unsupported reasoning: comparing unknown values"
    (_, Unknown) ->
      mrgThrowError "tensorValLt: Unsupported reasoning: comparing unknown values"
    (NonInf _, Inf rv) -> mrgReturn rv
    (Inf lv, NonInf _) -> mrgReturn $ symNot lv

-- | Checks if one tensor numerical value is greater than another.
tensorValGt ::
  (Mergeable a, SymOrd a) => TensorNum a -> TensorNum a -> ErrorEnv SymBool
tensorValGt = flip tensorValLt

-- | Checks if one tensor numerical value is less than or equal to another.
tensorValLe ::
  (Mergeable a, SymOrd a) => TensorNum a -> TensorNum a -> ErrorEnv SymBool
tensorValLe l r = mrgFmap symNot $ tensorValLt r l

-- | Checks if one tensor numerical value is greater than or equal to another.
tensorValGe ::
  (Mergeable a, SymOrd a) => TensorNum a -> TensorNum a -> ErrorEnv SymBool
tensorValGe l r = mrgFmap symNot $ tensorValLt l r

instance (IsString a, Mergeable a) => IsString (TensorNum a) where
  fromString = nonInf . fromString

class TensorExp a where
  -- | Computes the exponential of a tensor numerical value.
  -- This is currently only supported for real tensor values
  tensorExp :: TensorNum a -> TensorNum a

instance TensorExp SymInteger where
  tensorExp = error "Not supported"

instance TensorExp SymAlgReal where
  tensorExp (TensorNum l) = TensorNum $ do
    l1 <- l
    case l1 of
      NonInf lv -> mrgReturn $ NonInf $ exp lv
      Inf lv -> mrgIf lv (return $ Inf lv) (return $ NonInf 0)
      Unknown -> mrgReturn Unknown

class TensorDivMod a where
  -- | Divides one tensor numerical value by another.
  tensorDiv :: TensorNum a -> TensorNum a -> TensorNum a
  -- | Computes the quotient of one tensor numerical value by another.
  tensorMod :: TensorNum a -> TensorNum a -> TensorNum a
  -- | Computes the remainder of one tensor numerical value by another.
  tensorRem :: TensorNum a -> TensorNum a -> TensorNum a

instance TensorDivMod SymInteger where
  tensorDiv (TensorNum l) (TensorNum r) = TensorNum $ do
    l1 <- l
    r1 <- r
    case (l1, r1) of
      (NonInf lv, NonInf rv) -> mrgReturn $ NonInf $ divOr 0 lv rv
      -- (Inf lv, Inf rv) -> mrgReturn $ Unknown
      -- (NonInf lv, Inf rv) -> mrgReturn $ Inf rv
      -- (Inf lv, NonInf rv) -> mrgReturn $ Inf lv
      -- TODO: Overly approximated
      _ -> mrgReturn Unknown
  tensorMod (TensorNum l) (TensorNum r) = TensorNum $ do
    l1 <- l
    r1 <- r
    case (l1, r1) of
      (NonInf lv, NonInf rv) -> mrgReturn $ NonInf $ modOr lv lv rv
      -- (Inf lv, Inf rv) -> mrgReturn $ Unknown
      -- (NonInf lv, Inf rv) -> mrgReturn $ Inf rv
      -- (Inf lv, NonInf rv) -> mrgReturn $ Inf lv
      -- TODO: Overly approximated
      _ -> mrgReturn Unknown
  tensorRem (TensorNum l) (TensorNum r) = TensorNum $ do
    l1 <- l
    r1 <- r
    case (l1, r1) of
      (NonInf lv, NonInf rv) -> mrgReturn $ NonInf $ remOr lv lv rv
      -- (Inf lv, Inf rv) -> mrgReturn $ Unknown
      -- (NonInf lv, Inf rv) -> mrgReturn $ Inf rv
      -- (Inf lv, NonInf rv) -> mrgReturn $ Inf lv
      -- TODO: Overly approximated
      _ -> mrgReturn Unknown

instance TensorDivMod SymAlgReal where
  tensorDiv (TensorNum l) (TensorNum r) = TensorNum $ do
    l1 <- l
    r1 <- r
    case (l1, r1) of
      (NonInf lv, NonInf rv) -> mrgReturn $ NonInf $ fdivOr 0 lv rv
      -- (Inf lv, Inf rv) -> mrgReturn $ Unknown
      -- (NonInf lv, Inf rv) -> mrgReturn $ Inf rv
      -- (Inf lv, NonInf rv) -> mrgReturn $ Inf lv
      -- TODO: Overly approximated
      _ -> mrgReturn Unknown
  tensorMod = error "Not supported"
  tensorRem = error "Not supported"

type IsTensorNum a =
  ( Num a,
    SymOrd a,
    ITEOp a,
    SimpleMergeable a,
    TensorDivMod a,
    TensorExp a
  )
