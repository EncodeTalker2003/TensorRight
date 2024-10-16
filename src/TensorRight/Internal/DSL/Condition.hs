{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TensorRight.Internal.DSL.Condition
  ( Condition (..),
    zipCondition,
    elementWiseCondition,
    pointWiseCondition,
    elementWiseArith,
    unaryCondition,
    pointWiseArith,
    unaryArith,
  )
where

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Grisette
  ( LogicalOp ((.&&)),
    PPrint (pformat),
    Solvable (con),
    SymBool,
    SymInteger,
    symAll,
  )
import Prettyprinter ((<+>))
import TensorRight.Internal.DSL.Identifier (MapIdentifier)

data Condition = Condition
  { maps :: [MapIdentifier],
    condition :: [HM.HashMap T.Text SymInteger] -> SymBool
  }

instance Show Condition where
  show Condition {..} = "Condition [" <> show maps <> "]"

instance PPrint Condition where
  pformat Condition {..} = "Condition" <+> pformat maps

-- | The condition will be applied to each group of the elements with the same
-- axes.
--
-- For instance, @'zipCondition' (\l -> sum l .== 10) [a, b, c]@, checks if for
-- every key @k@, @a[k] + b[k] + c[k] .== 10@. This also means that
-- 'zipCondition' can only be used on maps having the same domain, i.e., the
-- same @Adim@.
zipCondition ::
  ([SymInteger] -> SymBool) ->
  [HM.HashMap T.Text SymInteger] ->
  SymBool
zipCondition _ [] = con True
zipCondition f allMaps@(m : _) = allSameKeys .&& symAll (f . byKey) keys
  where
    allSame [] = con True
    allSame (x : xs) = symAll (con . (== x)) xs .&& allSame xs
    allSameKeys = allSame (map HM.keys allMaps)
    keys = HM.keys m

    byKey :: T.Text -> [SymInteger]
    byKey key = fmap (HM.! key) allMaps

-- | The condition will be applied to two maps in an element-wise way.
--
-- For instance, @'elementWiseCondition' (.==) a b@, checks if for every key
-- @k@, @a[k] .== b[k]@. This also means that 'elementWiseCondition' can only be
-- used on maps having the same domain, i.e., the same @Adim@.
--
-- For @f@, users can use the already avaiable functions in Grisette like
-- 'Grisette..==', 'Grisette../=', 'Grisette..>', 'Grisette..>=', 'Grisette..<',
-- 'Grisette..<=', or they can create their own functions on the fly, as long as
-- it satisifies the signature. For example, the following precondition checks
-- if @m1 <= m2 || m1 == 1@:
--
-- @
-- let compareFunc x y = x <= y .|| x .== 1
-- 'TensorRight.precondition' [m1, m2] $ \[m1, m2] -> 'elementWiseCondition' compareFunc m1 m2
-- @
elementWiseCondition ::
  -- | Element-wise function
  (SymInteger -> SymInteger -> SymBool) ->
  -- | Left-hand side
  HM.HashMap T.Text SymInteger ->
  -- | Right-hand side
  HM.HashMap T.Text SymInteger ->
  -- | Resulting condition
  SymBool
elementWiseCondition f a b =
  con (HM.keysSet a == HM.keysSet b)
    .&& foldr (.&&) (con True) (HM.intersectionWith f a b)

-- | The condition will be applied to each element of the map.
--
-- For instance, @'unaryCondition' (.== 0) a@, checks if for every key @k@,
-- @a[k] .== 0@.
--
-- The user can use any function @f@, as long as it satisfies the signature. The
-- same condition could have been writen using 'pointWiseCondition'
--
-- @
-- 'TensorRight.precondition' [m] $ \[m] -> 'unaryCondition' (.> 1) m
-- 'TensorRight.precondition' [m] $ \[m] -> 'pointWiseCondition' (.>) m 1 -- equivalent
-- @
unaryCondition ::
  (SymInteger -> SymBool) ->
  HM.HashMap T.Text SymInteger ->
  SymBool
unaryCondition f a = foldr (.&&) (con True) (HM.map f a)

-- | The condition will be applied to two maps in a point-wise way.
-- 
-- For instance, @'pointWiseCondition' (.==) a 1@, checks if for every key @k@,
-- @a[k] .== 1@.
--
-- The user can use any function @f@, as long as it satisfies the signature.
-- For instance, here is how we can check if @m > 1@:
--
-- @
-- 'TensorRight.precondition' [m] $ \[m] -> pointWiseCondition (.>) m 1
-- @
pointWiseCondition ::
  (SymInteger -> SymInteger -> SymBool) ->
  HM.HashMap T.Text SymInteger ->
  SymInteger ->
  SymBool
pointWiseCondition f a b =
  foldr (.&&) (con True) (HM.map (`f` b) a)

-- | Helper for operate on two maps in an element-wise way.
--
-- For instance, @'elementWiseArith' (+) a b@, return a map @r@ such that for
-- every key @k@, @r[k] = a[k] + b[k]@. This also means that 'elementWiseArith'
-- can only be used on maps having the same domain, i.e., the same @Adim@.
-- 
-- For @f@, users can use any binary operator, as long as it satisifies the
-- signature. This is useful if you have a condition involving more than two
-- maps. For instance, this checks if @m1 == m2 + m3@
--
-- @
-- 'TensorRight.precondition' [m1, m2, m3] $
--   \[m1, m2, m3] -> 'elementWiseCondition' (.==) m1 ('elementWiseArith' (+) m2 m3) 
-- @
elementWiseArith ::
  (SymInteger -> SymInteger -> SymInteger) ->
  HM.HashMap T.Text SymInteger ->
  HM.HashMap T.Text SymInteger ->
  HM.HashMap T.Text SymInteger
elementWiseArith = HM.intersectionWith

-- | Helper for operate on two maps in a pointwise way. For instance,
-- @'pointWiseArith' (+) a v@, return a map @r@ such that for every key @k@,
-- @r[k] = a[k] + v@.
--
-- For @f@, users can use any binary operator, as long as it satisifies the
-- signature. For instance, this checks if @m1 == m2 + 2@
--
-- @
-- 'TensorRight.precondition' [m1, m2] $
--   \[m1, m2] -> 'elementWiseCondition' (.==) m1 ('pointWiseArith' (+) m2 2)
-- @
pointWiseArith ::
  (SymInteger -> SymInteger -> SymInteger) ->
  HM.HashMap T.Text SymInteger ->
  SymInteger ->
  HM.HashMap T.Text SymInteger
pointWiseArith f a b = HM.map (`f` b) a

-- | Helper for operate on a map in a unary way.
--
-- For instance, @'unaryWiseArith' (+2) a v@, return a map @r@ such that for
-- every key @k@, @r[k] = a[k] + 2@.
-- 
-- For @f@, users can use any binary operator, as long as it satisifies the
-- signature. For instance, this checks if @m1 == -m2@ (there are multiple ways
-- to express it)
--
-- @
-- 'TensorRight.precondition' [m1, m2] $
--   \[m1, m2] -> 'elementWiseCondition' (.==) m1 ('unaryArithOp' negate m2)
-- 
-- 'TensorRight.precondition' [m1, m2] $
--   \[m1, m2] -> 'elementWiseCondition' (\x y -> x .== negate y) m1 m2 -- equivalent
-- @
unaryArith ::
  (SymInteger -> SymInteger) ->
  HM.HashMap T.Text SymInteger ->
  HM.HashMap T.Text SymInteger
unaryArith = HM.map
