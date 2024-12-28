{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TensorRight.Internal.DSL.Condition
  ( Condition (..),
    zipCondition,
    elemWiseCond,
    elemWiseArith,
    unaryCond,
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
-- same @RClass@.
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
-- For instance, @'elemWiseCond' (.==) a b@, checks if for every key
-- @k@, @a[k] .== b[k]@. This also means that 'elemWiseCond' can only be
-- used on maps having the same domain, i.e., the same @RClass@.
--
-- For @f@, users can use the already avaiable functions in Grisette like
-- 'Grisette..==', 'Grisette../=', 'Grisette..>', 'Grisette..>=', 'Grisette..<',
-- 'Grisette..<=', or they can create their own functions on the fly, as long as
-- it satisifies the signature. For example, the following precondition checks
-- if @m1 <= m2 || m1 == 1@:
--
-- @
-- let compareFunc x y = x <= y .|| x .== 1
-- 'TensorRight.precondition'' [m1, m2] $ \[m1, m2] -> 'elemWiseCond' (.<=) m1 m2 .|| 'unaryCond' (.== 1) m1
-- @
elemWiseCond ::
  -- | Element-wise function
  (SymInteger -> SymInteger -> SymBool) ->
  -- | Left-hand side
  HM.HashMap T.Text SymInteger ->
  -- | Right-hand side
  HM.HashMap T.Text SymInteger ->
  -- | Resulting condition
  SymBool
elemWiseCond f a b =
  con (HM.keysSet a == HM.keysSet b)
    .&& foldr (.&&) (con True) (HM.intersectionWith f a b)

-- | The condition will be applied to each element of the map.
--
-- For instance, @'unaryCond' (.== 0) a@, checks if for every key @k@,
-- @a[k] .== 0@.
--
-- The user can use any function @f@, as long as it satisfies the signature.
--
-- @
-- 'TensorRight.precondition'' [m] $ \[m] -> 'unaryCond' (.> 1) m
-- @
unaryCond ::
  (SymInteger -> SymBool) ->
  HM.HashMap T.Text SymInteger ->
  SymBool
unaryCond f a = foldr (.&&) (con True) (HM.map f a)

-- | Helper for operate on two maps in an element-wise way.
--
-- For instance, @'elemWiseArith' (+) a b@, returns a map @r@ such that for
-- every key @k@, @r[k] = a[k] + b[k]@. This also means that 'elemWiseArith'
-- can only be used on maps having the same domain, i.e., the same @RClass@.
-- 
-- For @f@, users can use any binary operator, as long as it satisifies the
-- signature. This is useful if you have a condition involving more than two
-- maps. For instance, this checks if @m1 == m2 + m3@
--
-- @
-- 'TensorRight.precondition'' [m1, m2, m3] $
--   \[m1, m2, m3] -> 'elemWiseCond' (.==) m1 ('elemWiseArith' (+) m2 m3) 
-- @
elemWiseArith ::
  (SymInteger -> SymInteger -> SymInteger) ->
  HM.HashMap T.Text SymInteger ->
  HM.HashMap T.Text SymInteger ->
  HM.HashMap T.Text SymInteger
elemWiseArith = HM.intersectionWith

-- | Helper for operating on a map in a unary way.
--
-- For instance, @'unaryArith' (+2) a@, return a map @r@ such that for
-- every key @k@, @r[k] = a[k] + 2@.
-- 
-- For @f@, users can use any binary operator, as long as it satisifies the
-- signature. For instance, this checks if @m1 == m2 + 2@
--
-- @
-- 'TensorRight.precondition'' [m1, m2] $
--   \[m1, m2] -> 'elemWiseCond' (.==) m1 ('unaryArith' (+2) m2)
-- @
unaryArith ::
  (SymInteger -> SymInteger) ->
  HM.HashMap T.Text SymInteger ->
  HM.HashMap T.Text SymInteger
unaryArith = HM.map
