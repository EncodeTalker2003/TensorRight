module Main (main) where

import Grisette hiding ((-->))
import TensorRight

rule01 :: forall a. AnyDTypeRule a
rule01 _ = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  t <- newTensor @a "t" [adim --> map]
  lhs <- reverseTensor t [ByAdim adim]
  let rhs = t
  precondition [map] $ \[map'] -> map' .== 1
  rewrite
    "Reverse(A,dims) ⇒ A"
    lhs
    rhs

rule02 :: forall a. AnyDTypeRule a
rule02 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  map0 <- newMap "map0" adim0
  map1 <- newMap "map1" adim1
  t <- newTensor @a "t" [adim0 --> map0, adim1 --> map1]
  lhs <- reverseTensor (reverseTensor t [ByAdim adim0]) [ByAdim adim0]
  let rhs = t
  rewrite "Reverse(Reverse(A,dims1),dims2) ⇒ A" lhs rhs

rule03 :: forall a. AnyDTypeRule a
rule03 _ = do
  [adim0, adim1, adim2] <- newAdims ["adim0", "adim1", "adim2"]
  map0 <- newMap "map0" adim0
  map1 <- newMap "map1" adim1
  map2 <- newMap "map2" adim2
  t <- newTensor @a "t" [adim0 --> map0, adim1 --> map1, adim2 --> map2]
  lhs <-
    reverseTensor
      (reverseTensor t [ByAdim adim0, ByAdim adim1])
      [ByAdim adim1, ByAdim adim2]
  rhs <- reverseTensor t [ByAdim adim2, ByAdim adim0]
  rewrite
    "Reverse(Reverse(A,dims1),dims2) ⇒ Reverse(A, disjoint union of dims1 and dims2)"
    lhs
    rhs

rule04 :: forall a. NumRule a
rule04 _ = do
  [adim0] <- newAdims ["adim0"]
  map0 <- newMap "map0" adim0
  t <- newTensor @a "t" [adim0 --> map0]
  const <- constant @a "const" [adim0 --> map0]
  lhs <- reverseTensor (numBinOp Add t const) [ByAdim adim0]
  rhs <- numBinOp Add (reverseTensor t [ByAdim adim0]) const
  rewrite
    "Reverse(Binary(A,Const)) ⇒ Binary(Reverse(A),Const)"
    lhs
    rhs

main :: IO ()
main = do
  print "############################## rule01 ##############################"
  verifyAnyDTypeDSL rule01
  print "############################## rule02 ##############################"
  verifyAnyDTypeDSL rule02
  print "############################## rule03 ##############################"
  verifyAnyDTypeDSL rule03
  print "############################## rule04 ##############################"
  verifyNumDSL rule04
