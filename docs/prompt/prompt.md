There is a paper for verifying tensor rewrite rules. Please learn to write the DSL in it. This DSL is a Haskell-style one, and it uses monads. After finishing reading it. Please write codes for representing the following rewrite rule:

The rule tries to capture the case of pad(broadcast(x)), where x->shape().dimensions(), or broadcast(x)->dimensions(), is a subset of the padded dimensions in pad->config(), and the padded dimensions in pad->config() is in turn a strict subset of broadcast->shape().dimensions(). The combined op can be rewritten to broadcast2(pad(broadcast1(x))), where broadcast1 extends x  with dimensions that need to be padded, and broadcast2 extends the result of padding to full dimensions.


Note that:
- When doing binary operations like multiplication on numeric constants like 0 and 1, you should treat use something like `constant @a 0 [rclass --> shapeMap]` to represent the constant 0, instead of just using the number 0 directly. 
- You should use `numUnaryOp Neg X` to represent `Neg(X)`.

Here is an example of the work you need to do. Consider the following rewrite rule:

Add(Add(A, c1), c2) = Add(A, Add(c1, c2))

One version of the DSL code for it is:

```haskell
module Main (main) where

import Grisette hiding ((-->))
import TensorRight

rule01 :: forall a. NumRule a
rule01 _ = do
  rclass <- newRClass "rclass"
  map <- newMap "map" rclass
  tA <- newTensor @a "A" [rclass --> map]
  c1 <- constant @a "c1" [rclass --> map]
  c2 <- constant @a "c2" [rclass --> map]
  lhs <- numBinOp Add (numBinOp Add tA c1) c2
  rhs <- numBinOp Add tA (numBinOp Add c1 c2)
  rewrite "Add(Add(A, c1), c2) ⇒ Add(A, Add(c1, c2))" lhs rhs

main :: IO ()
main = do
  verifyNumDSL rule01

```

Here is another example of the work you need to do. Consider the following rewrite rule:

Select(P, A, A) = A

One version of the DSL code for it is:

```haskell
module Main (main) where

import Grisette hiding ((-->))
import TensorRight

rule01 :: forall a. AnyDTypeRule a
rule01 _ = do
  rclass <- newRClass "rclass"
  map <- newMap "map" rclass
  tP <- newTensor @SymBool "P" [rclass --> map]
  tA <- newTensor @a "A" [rclass --> map]
  lhs <- select tP tA tA
  let rhs = tA
  rewrite "Select(P, A, A) ⇒ A" lhs rhs

main :: IO ()
main = do
  verifyNumDSL rule01

```

Here is an example of the work you need to do. Consider the following rewrite rule:

Slice(Concat(A, B), A.size, A.size + B.size, 1) ⇒ B

One version of the DSL code for it is:

```haskell
rule08 :: forall a. AnyDTypeRule a
rule08 _ = do
  [rclass0, rclass1] <- newRClasses ["rclass0", "rclass1"]
  [rc0SizeA, rc0SizeB, rc0Start, rc0End, rc0Stride] <-
    newMaps ["rc0SizeA", "rc0SizeB", "rc0Start", "rc0End", "rc0Stride"] rclass0
  [rc1SizeA, rc1SizeB, rc1Start, rc1End, rc1Stride] <-
    newMaps ["rc1SizeA", "rc1SizeB", "rc1Start", "rc1End", "rc1Stride"] rclass1
  tA <- newTensor @a "A" [rclass0 --> rc0SizeA, rclass1 --> rc1SizeA]
  tB <- newTensor @a "B" [rclass0 --> rc0SizeB, rclass1 --> rc1SizeB]

  lhs <-
    slice (concatTensor tA tB (ByRClass rclass0)) $
      Slice
        { start = [rclass0 --> rc0Start, rclass1 --> rc1Start],
          end = [rclass0 --> rc0End, rclass1 --> rc1End],
          strides = [rclass0 --> rc0Stride, rclass1 --> rc1Stride]
        }
  precondition [rc0SizeA, rc0Start] $ \[a, s] -> a .== s
  precondition [rc0SizeA, rc0SizeB, rc0End] $
    \[a, b, e] -> e .== a + b
  precondition [rc0Stride] $ \[p] -> p .== 1

  rhs <-
    slice tB $
      Slice
        { start = [rclass1 --> rc1Start],
          end = [rclass1 --> rc1End],
          strides = [rclass1 --> rc1Stride]
        }
  rewrite "Slice(Concat(A, B), A.size, A.size + B.size, 1) ⇒ B" lhs rhs
```

Here is another example of the work you need to do. Consider the following rewrite rule:

Interior padding on one sized dimensions have no effect. As a result it makes other simplifications possible if there is no interior padding.

```haskell
rule02 :: forall a. AnyDTypeRule a
rule02 _ = do
  [rclass0, rclass1] <- newRClasses ["rclass0", "rclass1"]
  [rc0Size, rc0Low, rc0Int, rc0High] <-
    newMaps ["rc0Size", "rc0Low", "rc0Int", "rc0High"] rclass0
  [rc1Size, rc1Low, rc1Int, rc1High] <-
    newMaps ["rc1Size", "rc1Low", "rc1Int", "rc1High"] rclass1

  tA <- newTensor @a "A" [rclass0 --> rc0Size, rclass1 --> rc1Size]
  lhs <-
    pad tA ("a" :: a) $
      Padding
        { low = [rclass0 --> rc0Low, rclass1 --> rc1Low],
          high = [rclass0 --> rc0High, rclass1 --> rc1High],
          interior = [rclass0 --> rc0Int, rclass1 --> rc1Int]
        }
  precondition [rc1Size] $ \[size1] -> size1 .== 1

  rc1NewInt <- newConstMap "rc1NewInt" 0 rclass1
  rhs <-
    pad tA ("a" :: a) $
      Padding
        { low = [rclass0 --> rc0Low, rclass1 --> rc1Low],
          high = [rclass0 --> rc0High, rclass1 --> rc1High],
          interior = [rclass0 --> rc0Int, rclass1 --> rc1NewInt]
        }

  rewrite "Pad(A, val, low_high_int) ⇒ Pad(A, val, low_high_0)" lhs rhs
```

Here is another example of the work you need to do. Consider the following rewrite rule:

Broadcast(Broadcast(A, shape, dims), shape2, dims2) ⇒ Broadcast(A, shape3, dims3)

```haskell

rule08 :: forall a. AnyDTypeRule a
rule08 _ = do
  [rclass0, rclass1, rclass2] <- newRClasses ["rclass0", "rclass1", "rclass2"]
  rc0Size <- newMap "rc0Size" rclass0
  rc1Size <- newMap "rc1Size" rclass1
  rc2Size <- newMap "rc2Size" rclass2
  tA <- newTensor @a "A" [rclass0 --> rc0Size]
  lhs <- broadcast (broadcast tA [rclass1 --> rc1Size]) [rclass2 --> rc2Size]
  rhs <- broadcast tA [rclass1 --> rc1Size, rclass2 --> rc2Size]
  rewrite "Broadcast(Broadcast(A, shape, dims), shape2, dims2) ⇒ Broadcast(A, shape3, dims3)" lhs rhs
```