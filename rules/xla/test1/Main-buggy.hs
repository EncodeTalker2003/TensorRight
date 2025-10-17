-- This file defines a tensor rewrite rule using the TensorRight DSL.
-- The rule represents the algebraic factorization:
-- Add(X, Mul(X, Y)) ==> Mul(X, Add(1, Y))
-- It is assumed that this code is compiled in an environment where the
-- TensorRight DSL library and its functions (newRClass, newTensor, etc.)
-- are available.

module Main (main) where

import Grisette hiding ((-->))
import TensorRight

-- The rule is defined as a function with the 'NumRule' constraint,
-- which makes it polymorphic over numeric types (e.g., integers, floats).
factorRule :: forall a. NumRule a
factorRule _ = do
  -- A 'do' block is used because the DSL is monadic, allowing for the
  -- sequential definition of symbolic variables and expressions.

  -- 1. Define symbolic properties for the input tensors.
  -- We declare a single Rank Class (RClass). Tensors X and Y must have
  -- compatible shapes for element-wise operations, so their axes can be
  -- grouped into the same RClass. This ensures they are instantiated
  -- with the same rank during verification.
  rclass <- newRClass "rclass"

  -- We declare a Map on the RClass to represent the symbolic size of each
  -- axis. Using the same map for both tensors ensures their dimensions match.
  shapeMap <- newMap "shapeMap" rclass

  -- 2. Declare the input tensors.
  -- 'tX' is a new symbolic tensor named "X". Its shape is defined by the
  -- rank class and the shape map.
  tX <- newTensor @a "X" [rclass --> shapeMap]

  -- 'tY' is another symbolic tensor named "Y", defined with the same
  -- shape properties as 'tX' to ensure compatibility.
  tY <- newTensor @a "Y" [rclass --> shapeMap]

  -- 3. Construct the Left-Hand Side (LHS) of the rule.
  -- The expression is Add(X, Mul(X, Y)).
  -- We use 'numBinOp' for element-wise binary operations like 'Add' and 'Mul'.
  mul_xy <- numBinOp Mul tX tY
  lhs    <- numBinOp Add tX mul_xy

  -- 4. Construct the Right-Hand Side (RHS) of the rule.
  -- The expression is Mul(X, Add(1, Y)).
  -- We assume the DSL's 'Num' instance for the monad allows numeric literals
  -- like '1' to be automatically lifted into a broadcastable scalar constant tensor.
  add_1y <- numBinOp Add 1 tY
  -- ***
  -- Another buggy version
  -- one <- constant @a 1 [rcls --> shapeMap]
  -- rhs <- numBinOp Mul tX (numBinOp Add one tY)
  rhs    <- numBinOp Mul tX add_1y

  -- 5. Define the rewrite.
  -- This ties the LHS and RHS together under a descriptive name. The TENSORRIGHT
  -- system will attempt to prove that LHS is equivalent to RHS.
  rewrite "Add(X, Mul(X, Y)) ==> Mul(X, Add(1, Y))" lhs rhs

-- The main function to execute the verification of the rule.
main :: IO ()
main = do
  -- 'verifyNumDSL' is the entry point for the TENSORRIGHT verification engine.
  -- It takes the rule definition and attempts to prove its correctness
  -- for tensors of arbitrary rank and size.
  verifyNumDSL factorRule
