module Main (main) where

import Grisette hiding (dot, (-->))
import TensorRight

rule01 :: forall a. NumRule a
rule01 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  [adim0size, adim0exsize, adim0lhssi, adim0rhssi] <-
    newMaps ["adim0size", "adim0exsize", "adim0lhssi", "adim0rhssi"] adim0
  [adim1size, adim1exsize, adim1lhssi, adim1rhssi] <-
    newMaps ["adim1size", "adim1exsize", "adim1lhssi", "adim1rhssi"] adim1
  tensor0 <- newTensor @a "tensor0" [adim0 --> adim0size]
  tensor1 <- newTensor @a "tensor1" [adim1 --> adim1size]
  lhs <-
    numBinOp
      Mul
      (reduce tensor0 [adim0 --> adim0lhssi])
      (reduce tensor1 [adim1 --> adim1lhssi])
  rhs <-
    reduce
      ( numBinOp
          Mul
          (broadcast tensor0 [adim1 --> adim1exsize])
          (broadcast tensor1 [adim0 --> adim0exsize])
      )
      [adim0 --> adim0rhssi, adim1 --> adim1rhssi]
  precondition [adim0size, adim0exsize] $
    \[adim0size, adim0exsize] -> adim0size .== adim0exsize
  precondition [adim1size, adim1exsize] $
    \[adim1size, adim1exsize] -> adim1size .== adim1exsize
  siRelation [adim0lhssi, adim0rhssi] $
    \[adim0lhssi, adim0rhssi] -> adim0lhssi .== adim0rhssi
  siRelation [adim1lhssi, adim1rhssi] $
    \[adim1lhssi, adim1rhssi] -> adim1lhssi .== adim1rhssi
  checkSIMap [adim0lhssi, adim1lhssi] [adim0rhssi, adim1rhssi]
  rewrite
    "Mul(Reduce(X), Reduce(Y)) ⇒ Reduce(Mul(Broadcast(X), Broadcast(Y)))"
    lhs
    rhs

rule02 :: forall a. NumRule a
rule02 _ = do
  [concatAdim, otherAdim] <- newAdims ["concatAdim", "otherAdim"]
  [concatSize0, concatSize1] <-
    newMaps ["concatSize0", "concatSize1"] concatAdim
  singletonConcat <- newConstMap "singletonConcat" 1 concatAdim
  [otherSize] <- newMaps ["otherSize"] otherAdim
  [lhsSIInnerX, lhsSIInnerY, lhsSIOuter, rhsSI] <-
    newMaps ["lhsSIInnerX", "lhsSIInnerY", "lhsSIOuter", "rhsSI"] concatAdim
  x <- newTensor @a "x" [concatAdim --> concatSize0, otherAdim --> otherSize]
  y <- newTensor @a "y" [concatAdim --> concatSize1, otherAdim --> otherSize]
  lhs <-
    reduce
      ( concatTensor
          ( broadcast
              (reduce x [concatAdim --> lhsSIInnerX])
              [concatAdim --> singletonConcat]
          )
          ( broadcast
              (reduce y [concatAdim --> lhsSIInnerY])
              [concatAdim --> singletonConcat]
          )
          (ByAdim concatAdim)
      )
      [concatAdim --> lhsSIOuter]
  rhs <- reduce (concatTensor x y (ByAdim concatAdim)) [concatAdim --> rhsSI]
  monitorMapOnFailure "concatSize0" (ByAdim concatAdim) concatSize0
  monitorMapOnFailure "concatSize1" (ByAdim concatAdim) concatSize1
  monitorMapOnFailure "lhsSIInnerX" (ByAdim concatAdim) lhsSIInnerX
  monitorMapOnFailure "lhsSIInnerY" (ByAdim concatAdim) lhsSIInnerY
  monitorMapOnFailure "lhsSIOuter" (ByAdim concatAdim) lhsSIOuter

  siRelation [lhsSIInnerX, lhsSIInnerY, lhsSIOuter, rhsSI, concatSize0, concatSize1] $
    \[lhsSIInnerX, lhsSIInnerY, lhsSIOuter, rhsSI, concatSize0, concatSize1] ->
      (lhsSIInnerX .>= 0 .&& lhsSIInnerX .< concatSize0)
        .&& (lhsSIInnerY .>= 0 .&& lhsSIInnerY .< concatSize1)
        .&& (lhsSIOuter .== 0 .|| lhsSIOuter .== 1)
        .&& symIte
          (lhsSIOuter .== 0)
          (lhsSIInnerX .== rhsSI)
          (concatSize0 + lhsSIInnerY .== rhsSI)
  checkSIMap [lhsSIInnerX, lhsSIInnerY, lhsSIOuter] [rhsSI]
  rewrite "Reduce(Concat(Reduce(X),Reduce(Y))) ⇒ Reduce(Concat(X,Y))" lhs rhs

rule03 :: forall a. NumRule a
rule03 _ = do
  [reductionAdim, adim1] <- newAdims ["reductionAdim", "adim1"]
  [reductionSize] <- newMaps ["reductionSize"] reductionAdim
  [lhsSI, rhsSI] <- newMaps ["lhsSI", "rhsSI"] reductionAdim
  [otherSize] <- newMaps ["otherSize"] adim1
  x <- newTensor @a "x" [reductionAdim --> reductionSize, adim1 --> otherSize]
  lhs <-
    numBinOp
      Mul
      (constant @a "a" [adim1 --> otherSize])
      (reduce x [reductionAdim --> lhsSI])
  rhs <-
    reduce
      ( numBinOp
          Mul
          ( constant @a
              "a"
              [reductionAdim --> reductionSize, adim1 --> otherSize]
          )
          x
      )
      [reductionAdim --> rhsSI]
  siRelation [lhsSI, rhsSI] $ \[lhsSI, rhsSI] -> lhsSI .== rhsSI
  checkSIMap [lhsSI] [rhsSI]
  rewrite "Const * Reduce(X) ⇒ Reduce(Const * X)" lhs rhs

rule04 :: forall a. NumRule a
rule04 _ = do
  [reductionAdim, nonReductionAdim] <- newAdims ["reductionAdim", "nonReductionAdim"]
  [reductionSize] <-
    newMaps ["reductionSize"] reductionAdim
  [otherSize] <-
    newMaps ["otherSize"] nonReductionAdim
  [lhsSI, rhsSI] <- newMaps ["lhsSI", "rhsSI"] reductionAdim
  x <-
    newTensor @a
      "x"
      [ reductionAdim --> reductionSize @@ "l0",
        nonReductionAdim --> otherSize @@ "l1"
      ]
  lhs <-
    reduce
      ( relabel
          x
          [ ByLabel "l0" --> ByLabel "l1",
            ByLabel "l1" --> ByLabel "l0"
          ]
      )
      [ByLabel "l1" --> lhsSI]
  rhs <-
    relabel
      (reduce x [ByLabel "l0" --> rhsSI])
      [ByLabel "l1" --> ByLabel "l0"]
  siRelation [lhsSI, rhsSI] $ \[lhsSI, rhsSI] -> lhsSI .== rhsSI
  checkSIMap [lhsSI] [rhsSI]
  rewrite "Reduce(Relabel(X)) ⇒ Relabel(Reduce(X))" lhs rhs

rule05 :: forall a. NumRule a
rule05 _ = do
  [adim0, adim1, adim2] <- newAdims ["adim0", "adim1", "adim2"]
  [adim0size, adim0lsi, adim0rsi] <-
    newMaps ["adim0size", "adim0lsi", "adim0rsi"] adim0
  [adim1size, adim1lsi, adim1rsi] <-
    newMaps ["adim1size", "adim1lsi", "adim1rsi"] adim1
  adim2size <- newMap "adim2size" adim2
  x <-
    newTensor @a
      "x"
      [adim0 --> adim0size, adim1 --> adim1size, adim2 --> adim2size]
  lhs <- reduce (reduce x [adim0 --> adim0lsi]) [adim1 --> adim1lsi]
  rhs <- reduce x [adim0 --> adim0rsi, adim1 --> adim1rsi]
  siRelation [adim0lsi, adim0rsi] $
    \[adim0lsi, adim0rsi] -> adim0lsi .== adim0rsi
  siRelation [adim1lsi, adim1rsi] $
    \[adim1lsi, adim1rsi] -> adim1lsi .== adim1rsi
  checkSIMap [adim0lsi, adim1lsi] [adim0rsi, adim1rsi]
  rewrite "Reduce(Reduce(X)) ⇒ Reduce(X)" lhs rhs

rule06 :: forall a. NumRule a
rule06 _ = do
  [adim0, adim1, adim2, adim3] <- newAdims ["adim0", "adim1", "adim2", "adim3"]
  [adim0size, adim0lsi, adim0rsi] <-
    newMaps ["adim0size", "adim0lsi", "adim0rsi"] adim0
  [adim1size, adim1lsi, adim1rsi] <-
    newMaps ["adim1size", "adim1lsi", "adim1rsi"] adim1
  adim2size <- newMap "adim2size" adim2
  adim3size <- newMap "adim3size" adim3
  x <-
    newTensor @a
      "x"
      [adim0 --> adim0size, adim1 --> adim1size, adim2 --> adim2size]
  y <-
    newTensor @a
      "y"
      [adim0 --> adim0size, adim1 --> adim1size, adim3 --> adim3size]
  lhs <-
    reduce
      (dot x y [adim0 --> adim0lsi] [ByAdim adim1])
      [adim1 --> adim1lsi]
  rhs <- dot x y [adim0 --> adim0rsi, adim1 --> adim1rsi] []
  siRelation [adim0lsi, adim0rsi] $
    \[adim0lsi, adim0rsi] -> adim0lsi .== adim0rsi
  siRelation [adim1lsi, adim1rsi] $
    \[adim1lsi, adim1rsi] -> adim1lsi .== adim1rsi
  checkSIMap [adim0lsi, adim1lsi] [adim0rsi, adim1rsi]
  rewrite "Reduce(Dot(X,Y)) ⇒ Dot(X,Y)" lhs rhs

rule07 :: forall a. NumRule a
rule07 _ = do
  [adimDegenerate, adim1] <- newAdims ["adimDegenerate", "adim1"]
  adimDegenerateSize <- newConstMap "adimDegenerateSize" 1 adimDegenerate
  adim1Size <- newMap "adim1Size" adim1
  x <- newTensor @a "x" [adimDegenerate --> adimDegenerateSize, adim1 --> adim1Size]
  siDegenerate <- newMap "siDegenerate" adimDegenerate

  lhs <- reduce x [adimDegenerate --> siDegenerate]
  rhs <- reshapeDegenerate x [] [ByAdim adimDegenerate]

  siRelation [siDegenerate] $ \[siDegenerate] -> siDegenerate .== 0
  checkSIMap [siDegenerate] []
  rewrite "Reduce(X) ⇒ ReshapeDegenerate(X)" lhs rhs

rule08 :: forall a. NumRule a
rule08 _ = do
  [adimDegenerate, adim1] <- newAdims ["adimDegenerate", "adim1"]
  adimDegenerateSize <- newConstMap "adimDegenerateSize" 1 adimDegenerate
  adim1Size <- newMap "adim1Size" adim1
  x <- newTensor @a "x" [adimDegenerate --> adimDegenerateSize, adim1 --> adim1Size]
  siDegenerate <- newMap "siDegenerate" adimDegenerate

  lhs <-
    reduce
      (relabel x [adimDegenerate --> ByLabel "l0"])
      [ByLabel "l0" --> siDegenerate]
  rhs <- reshapeDegenerate x [] [ByAdim adimDegenerate]

  siRelation [siDegenerate] $ \[siDegenerate] -> siDegenerate .== 0
  checkSIMap [siDegenerate] []
  rewrite "Reduce(X) ⇒ ReshapeDegenerate(X)" lhs rhs

main :: IO ()
main = do
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
  print "############################## rule06 ##############################"
  verifyNumDSL rule06
  print "############################## rule07 ##############################"
  verifyNumDSL rule07
  print "############################## rule08 ##############################"
  verifyNumDSL rule08
