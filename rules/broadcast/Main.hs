module Main (main) where

import Grisette hiding ((-->))
import TensorRight

rule01 :: forall a. NumRule a
rule01 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  map0 <- newMap "map0" adim0
  map1 <- newMap "map1" adim1
  tensor0 <- newTensor @a "tensor0" [adim0 --> map0]
  tensor1 <- newTensor @a "tensor1" [adim0 --> map0]
  lhs <- numBinOp Add (broadcast tensor0 [adim1 --> map1]) (broadcast tensor1 [adim1 --> map1])
  rhs <- broadcast (numBinOp Add tensor0 tensor1) [adim1 --> map1]
  rewrite "Add(Broadcast(A),Broadcast(B)) ⇒ Broadcast(Add(A,B))" lhs rhs

rule02 :: forall a. NumRule a
rule02 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  map0 <- newMap "map0" adim0
  map1 <- newMap "map1" adim1
  tensor0 <- newTensor @a "tensor0" [adim0 --> map0]
  tensor1 <- newTensor @a "tensor1" [adim0 --> map0]
  lhs <- numBinOp Mul (broadcast tensor0 [adim1 --> map1]) (broadcast tensor1 [adim1 --> map1])
  rhs <- broadcast (numBinOp Mul tensor0 tensor1) [adim1 --> map1]
  rewrite "Mul(Broadcast(A),Broadcast(B)) ⇒ Broadcast(Mul(A,B))" lhs rhs

rule03 :: forall a. AnyDTypeRule a
rule03 _ = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  lhsTensor <- constant @a "a" [adim --> map @@ "label1"]
  rhsTensor <- constant @a "a" [adim --> map @@ "label2"]
  lhs <- relabel lhsTensor [ByLabel "label1" --> ByLabel "label2"]
  let rhs = rhsTensor
  rewrite "Transpose(Broadcast(Scalar)) ⇒ Broadcast(Scalar)" lhs rhs

rule04 :: forall a. AnyDTypeRule a
rule04 _ = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  lhs <- reverseTensor (constant @a "a" [adim --> map]) [ByAdim adim]
  rhs <- constant @a "a" [adim --> map]
  rewrite "Reverse(Broadcast(Scalar)) ⇒ Broadcast(Scalar)" lhs rhs

rule05 :: forall a. AnyDTypeRule a
rule05 _ = do
  adim <- newAdim "adim"
  [origSizeMap, newSizeMap, startMap, endMap, strideMap] <- newMaps ["origSizeMap", "newSizeMap", "startMap", "endMap", "strideMap"] adim
  lhs <- slice (constant @a "a" [adim --> origSizeMap]) [adim --> startMap] [adim --> endMap] [adim --> strideMap]
  rhs <- constant @a "a" [adim --> newSizeMap]
  precondition [startMap] $ \[start] -> start .>= 0
  precondition [strideMap] $ \[stride] -> stride .>= 1
  precondition [origSizeMap, endMap] $ \[origSize, end] -> end .<= origSize
  precondition [newSizeMap, startMap, endMap, strideMap] $
    \[newSize, start, end, stride] -> end .== start + newSize * stride
  rewrite "Slice(Broadcast(Scalar)) ⇒ Broadcast(Scalar)" lhs rhs

rule06 :: forall a. AnyDTypeRule a
rule06 _ = do
  adim <- newAdim "adim"
  [origSizeMap, newSizeMap, startMap] <- newMaps ["origSizeMap", "newSizeMap", "startMap"] adim
  lhs <- dynamicSlice (constant @a "a" [adim --> origSizeMap]) DySlice {start = [adim --> startMap], sizes = [adim --> newSizeMap]}
  rhs <- constant @a "a" [adim --> newSizeMap]
  precondition [startMap] $ \[start] -> start .>= 0
  precondition [origSizeMap, startMap, newSizeMap] $
    \[origSize, start, newSize] -> start + newSize .<= origSize
  rewrite "DynamicSlice(Broadcast(Scalar)) ⇒ Broadcast(Scalar)" lhs rhs

rule07 :: DSLContext Rewrite
rule07 = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  map0 <- newMap "map0" adim0
  map1 <- newMap "map1" adim1
  lhs <- broadcast (iota [adim0 --> map0] (ByAdim adim0)) [adim1 --> map1]
  rhs <- iota [adim0 --> map0, adim1 --> map1] (ByAdim adim0)
  rewrite "Broadcast(Iota) ⇒ Iota" lhs rhs

rule08 :: forall a. AnyDTypeRule a
rule08 _ = do
  [adim0, adim1, adim2] <- newAdims ["adim0", "adim1", "adim2"]
  map0 <- newMap "map0" adim0
  map1 <- newMap "map1" adim1
  map2 <- newMap "map2" adim2
  tensor <- newTensor @a "tensor" [adim0 --> map0]
  lhs <- broadcast (broadcast tensor [adim1 --> map1]) [adim2 --> map2]
  rhs <- broadcast tensor [adim1 --> map1, adim2 --> map2]
  rewrite "Broadcast(Broadcast(A, shape, dims), shape2, dims2) ⇒ Broadcast(A, shape3, dims3)" lhs rhs

main :: IO ()
main = do
  print "############################## rule01 ##############################"
  verifyNumDSL rule01
  print "############################## rule02 ##############################"
  verifyNumDSL rule02
  print "############################## rule03 ##############################"
  verifyAnyDTypeDSL rule03
  print "############################## rule04 ##############################"
  verifyAnyDTypeDSL rule04
  print "############################## rule05 ##############################"
  verifyAnyDTypeDSL rule05
  print "############################## rule06 ##############################"
  verifyAnyDTypeDSL rule06
  print "############################## rule07 ##############################"
  verifyDSL rule07
  print "############################## rule08 ##############################"
  verifyAnyDTypeDSL rule08
