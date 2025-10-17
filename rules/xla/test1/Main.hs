module Main (main) where

import Grisette hiding ((-->))
import TensorRight

-- This rule represents the distributive property: X + (X * Y) = X * (1 + Y)
-- It should hold for tensors of any rank and size.
rule00 :: forall a. NumRule a
rule00 _ = do
  -- Declare a rank class. All axes for all tensors in this rule will belong
  -- to this class, ensuring they can be instantiated to the same rank .
  rclass <- newRClass "rclass"

  -- Declare an abstract map for the shape. This map will define the symbolic
  -- size for each axis within the rank class .
  shapeMap <- newMap "shape" rclass

  -- Declare the input tensors X and Y. They are assigned the same shape
  -- structure, making them compatible for element-wise operations .
  tX <- newTensor @a "X" [rclass --> shapeMap]
  tY <- newTensor @a "Y" [rclass --> shapeMap]

  -- Declare a constant tensor with all elements equal to 1, with the same
  -- shape as X and Y. This uses the `const` operator described in the paper .
  one <- constant @a 1 [rclass --> shapeMap]

  -- Construct the Left-Hand Side (LHS) of the rule: Add(X, Mul(X, Y))
  mulXY <- numBinOp Mul tX tY
  lhs <- numBinOp Add tX mulXY

  -- Construct the Right-Hand Side (RHS) of the rule: Mul(X, Add(1, Y))
  add1Y <- numBinOp Add one tY
  rhs <- numBinOp Mul tX add1Y

  -- Define the rewrite rule for the verifier, giving it a descriptive name.
  rewrite "Add(X, Mul(X, Y)) ==> Mul(X, Add(1, Y))" lhs rhs

-- | This rule represents the algebraic simplification:
-- Neg(Mul(Neg(X), Y)) ==> Mul(X, Y)
-- It states that multiplying the negation of a tensor X with another tensor Y,
-- and then negating the result, is equivalent to multiplying X and Y directly.
rule01 :: forall a. NumRule a
rule01 _ = do
  -- For element-wise operations like Mul, the input tensors (X and Y) must have
  -- the same rank and shape. We define a single rank class and a single shape
  -- map to represent this constraint.
  rclass <- newRClass "rclass"
  shapeMap <- newMap "shapeMap" rclass

  -- Declare the symbolic input tensors X and Y. They are defined with the same
  -- rank class and shape map, ensuring they are compatible for multiplication.
  tX <- newTensor @a "X" [rclass --> shapeMap]
  tY <- newTensor @a "Y" [rclass --> shapeMap]

  -- Construct the Left-Hand Side (LHS) of the rule: Neg(Mul(Neg(X), Y))
  -- 1. `Neg(X)` is represented by `numUnaryOp Neg tX`.
  -- 2. `Mul(Neg(X), Y)` is `numBinOp Mul (numUnaryOp Neg tX) tY`.
  -- 3. `Neg(...)` wraps the multiplication result.
  let negX = numUnaryOp Neg tX
  let mulNegXY = numBinOp Mul negX tY
  lhs <- numUnaryOp Neg mulNegXY

  -- Construct the Right-Hand Side (RHS) of the rule: Mul(X, Y)
  rhs <- numBinOp Mul tX tY

  -- Define the rewrite rule to be verified, providing a descriptive name,
  -- and the symbolic expressions for the LHS and RHS.
  rewrite "Neg(Mul(Neg(X), Y)) ==> Mul(X, Y)" lhs rhs

-- | This rule represents the distribution of an addition over a select operation.
-- Add(Select(P, X, Y), Z) ==> Select(P, Add(X, Z), Add(Y, Z))
-- The type constraint 'NumRule a' is used because the 'Add' operator is numeric.
rule02 :: forall a. NumRule a
rule02 _ = do
  -- All tensors in this rule share the same shape, so we define one
  -- rank class and one shape map.
  rclass <- newRClass "rclass"
  shapeMap <- newMap "shapeMap" rclass

  -- Define the tensors involved in the rule.
  -- P is the boolean predicate tensor for the select operation.
  tP <- newTensor @SymBool "P" [rclass --> shapeMap]
  -- X, Y, and Z are numeric tensors of a polymorphic type 'a'.
  tX <- newTensor @a "X" [rclass --> shapeMap]
  tY <- newTensor @a "Y" [rclass --> shapeMap]
  tZ <- newTensor @a "Z" [rclass --> shapeMap]

  -- Construct the Left-Hand Side (LHS) of the rule.
  -- This corresponds to: Add(Select(P, X, Y), Z)
  selectRes <- select tP tX tY
  lhs <- numBinOp Add selectRes tZ

  -- Construct the Right-Hand Side (RHS) of the rule.
  -- This corresponds to: Select(P, Add(X, Z), Add(Y, Z))
  addXz <- numBinOp Add tX tZ
  addYz <- numBinOp Add tY tZ
  rhs <- select tP addXz addYz

  -- Create the rewrite rule with a descriptive name.
  rewrite "Add(Select(P,X,Y),Z) ==> Select(P,Add(X,Z),Add(Y,Z))" lhs rhs

-- Rule: Add(Slice(X, S, E, P), Slice(Y, S, E, P)) ==> Slice(Add(X, Y), S, E, P)
rule03 :: forall a. NumRule a
rule03 _ = do
  -- Declare a single rank class, as all axes in this rule share the same role.
  rcls <- newRClass "rcls"

  -- Declare abstract maps for the tensor shape (size) and the slice parameters
  -- (start, end, stride). These maps are defined over the rank class `rcls`.
  [size, start, end, stride] <- newMaps ["size", "start", "end", "stride"] rcls

  -- Declare two input tensors, X and Y. They must be addable, so they are defined
  -- with the same shape by using the same rank class and size map.
  tX <- newTensor @a "X" [rcls --> size]
  tY <- newTensor @a "Y" [rcls --> size]

  -- Define a record for the slice attributes. This ensures the same start, end,
  -- and stride parameters are used for all slice operations in the rule.
  let sliceAttrs =
        Slice
          { start = [rcls --> start],
            end = [rcls --> end],
            strides = [rcls --> stride]
          }

  -- Construct the Left-Hand Side (LHS) of the rewrite rule.
  -- First, slice tensor X and tensor Y independently.
  sliceX <- slice tX sliceAttrs
  sliceY <- slice tY sliceAttrs
  -- Then, add the results of the two slice operations.
  lhs <- numBinOp Add sliceX sliceY

  -- Construct the Right-Hand Side (RHS) of the rewrite rule.
  -- First, add the original tensors X and Y.
  addXY <- numBinOp Add tX tY
  -- Then, apply the slice operation to the result of the addition.
  rhs <- slice addXY sliceAttrs

  -- Create the rewrite rule object for the verifier.
  rewrite "Add(Slice(X, S, E, P), Slice(Y, S, E, P)) ==> Slice(Add(X, Y), S, E, P)" lhs rhs

rule04 :: forall a. NumRule a
rule04 _ = do
  -- Declare a rank class to represent a set of axes that can be instantiated
  -- to any rank. This makes the rule rank-polymorphic.
  rclass <- newRClass "rclass"

  -- Declare symbolic maps for the shape and padding dimensions (low, high, interior).
  -- These are defined on the rank class.
  [shape, low, high, int] <- newMaps ["shape", "low", "high", "int"] rclass

  -- Declare two input tensors, X and Y. They must have the same shape
  -- to be valid inputs for the Add operation.
  tX <- newTensor @a "X" [rclass --> shape]
  tY <- newTensor @a "Y" [rclass --> shape]

  -- Define the padding attributes structure to be used in the rule.
  let paddingAttrs = Padding
        { low = [rclass --> low],
          high = [rclass --> high],
          interior = [rclass --> int]
        }

  -- Construct the Left-Hand Side (LHS) of the rule.
  -- 1. First, add the two tensors X and Y.
  addXY <- numBinOp Add tX tY
  -- 2. Then, pad the result of the addition. The padding value is 0,
  --    satisfying the rule's precondition.
  lhs <- pad addXY (0 :: a) paddingAttrs

  -- Construct the Right-Hand Side (RHS) of the rule.
  -- 1. Pad tensor X with 0.
  padX <- pad tX (0 :: a) paddingAttrs
  -- 2. Pad tensor Y with 0.
  padY <- pad tY (0 :: a) paddingAttrs
  -- 3. Add the two padded tensors.
  rhs <- numBinOp Add padX padY

  -- Formally state the rewrite rule for the verifier.
  rewrite "Pad(Add(X, Y), 0, ...) ==> Add(Pad(X, 0, ...), Pad(Y, 0, ...))" lhs rhs


-- This rule models the transformation:
-- pad(broadcast(x)) ==> broadcast(pad(broadcast(x)))
-- It applies when the set of padded dimensions is a strict subset
-- of the dimensions added by the broadcast operation.
rule05 :: forall a. AnyDTypeRule a
rule05 _ = do
  -- 1. Declare three rank classes to partition the tensor axes.
  --    - rclass_x: The original dimensions of tensor X.
  --    - rclass_pad: Dimensions added by broadcast AND padded.
  --    - rclass_bcast: Dimensions added by broadcast but NOT padded.
  [rclass_x, rclass_pad, rclass_bcast] <-
    newRClasses ["rclass_x", "rclass_pad", "rclass_bcast"]

  -- 2. Declare symbolic maps for shapes and padding attributes for each rank class.
  x_shape <- newMap "x_shape" rclass_x
  pad_shape <- newMap "pad_shape" rclass_pad
  bcast_shape <- newMap "bcast_shape" rclass_bcast

  -- Padding attributes only apply to the dimensions that are actually padded.
  [x_low, x_high, x_int] <- newMaps ["x_low", "x_high", "x_int"] rclass_x
  [pad_low, pad_high, pad_int] <- newMaps ["pad_low", "pad_high", "pad_int"] rclass_pad

  -- 3. Define the input tensor 'X' with only the original dimensions.
  tX <- newTensor @a "X" [rclass_x --> x_shape]

  -- 4. Construct the Left-Hand Side (LHS): pad(broadcast(X))
  -- First, broadcast 'X' to the full shape, adding both padded and non-padded new dimensions.
  let bcast_lhs = broadcast tX [rclass_pad --> pad_shape, rclass_bcast --> bcast_shape]

  -- Create constant maps with value 0 for the non-padded dimensions (`rclass_bcast`).
  bcast_low <- newConstMap "bcast_low" 0 rclass_bcast
  bcast_high <- newConstMap "bcast_high" 0 rclass_bcast
  bcast_int <- newConstMap "bcast_int" 0 rclass_bcast

  -- Then, pad the broadcasted tensor.
  lhs <-
        pad bcast_lhs ("val" :: a) $
          Padding
            { low = [rclass_x --> x_low, rclass_pad --> pad_low, rclass_bcast --> bcast_low],
              high = [rclass_x --> x_high, rclass_pad --> pad_high, rclass_bcast --> bcast_high],
              interior = [rclass_x --> x_int, rclass_pad --> pad_int, rclass_bcast --> bcast_int]
            }

  -- 5. Construct the Right-Hand Side (RHS): broadcast2(pad(broadcast1(X)))
  -- First, perform `broadcast1`, adding only the dimensions that will be padded.
  let bcast1_rhs = broadcast tX [rclass_pad --> pad_shape]

  -- Then, pad the result. The padding attributes apply to the original and newly added padded dimensions.
  let pad_rhs =
        pad bcast1_rhs ("val" :: a) $
          Padding
            { low = [rclass_x --> x_low, rclass_pad --> pad_low],
              high = [rclass_x --> x_high, rclass_pad --> pad_high],
              interior = [rclass_x --> x_int, rclass_pad --> pad_int]
            }

  -- Finally, perform `broadcast2`, adding the remaining non-padded dimensions.
  rhs <- broadcast pad_rhs [rclass_bcast --> bcast_shape]

  -- 6. State the rewrite rule for verification.
  rewrite "Pad(Broadcast(x)) ==> Broadcast(Pad(Broadcast(x)))" lhs rhs

main :: IO ()
main = do
  -- Call the main verification routine on the defined rule .
  -- TensorRight will infer the necessary bounds and discharge proof
  -- obligations to an SMT solver to verify the rule in the unbounded setting .
  print "############################## rule00 ##############################"
  verifyNumDSL rule00
  print "############################## rule01 ##############################"
  verifyNumDSL rule01
  print "############################## rule02 ##############################"
  verifyNumDSL rule02
  print "############################## rule03 ##############################"
  verifyNumDSL rule03
  print "############################## rule04 ##############################"
  verifyNumDSL rule04
  print "############################## rule05 ##############################"
  verifyNumDSL rule05