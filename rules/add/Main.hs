module Main (main) where

import Grisette hiding ((-->))
import TensorRight

rule01 :: forall a. NumRule a
rule01 _ = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  tensor <- newTensor @a "tensor" [adim --> map]
  constTensor1 <- constant @a "a" [adim --> map]
  constTensor2 <- constant @a "b" [adim --> map]
  lhs <- numBinOp Add (numBinOp Add tensor constTensor1) constTensor2
  rhs <- numBinOp Add tensor (numBinOp Add constTensor1 constTensor2)
  rewrite "Add(Add(A,Const), Const2) ⇒ Add(A,Add(Const,Const2))" lhs rhs

rule02 :: forall a. NumRule a
rule02 _ = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  tensor <- newTensor @a "tensor" [adim --> map]
  constTensor <- constant @a 0 [adim --> map]
  lhs <- numBinOp Add tensor constTensor
  let rhs = tensor
  rewrite "Add(A,0)⇒A" lhs rhs

rule03 :: forall a. NumRule a
rule03 _ = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  tensor <- newTensor @a "tensor" [adim --> map]
  constTensor <- constant @a 0 [adim --> map]
  lhs <- numBinOp Add constTensor tensor
  let rhs = tensor
  rewrite "Add(0,A)⇒A" lhs rhs

rule04 :: forall a. NumRule a
rule04 _ = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  t1 <- newTensor @a "t1" [adim --> map]
  t2 <- constant @a "a" [adim --> map]
  lhs <- numBinOp Add t1 t2
  rhs <- numBinOp Add t2 t1
  rewrite "Add(Const,A) ⇒ Add(A,Const)" lhs rhs

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
