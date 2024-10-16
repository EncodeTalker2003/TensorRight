module Main (main) where

import Grisette hiding ((-->))
import TensorRight
import TensorRight.Internal.Core.Tensor.TensorInt (tensorValLt)
import Control.Monad.Except (runExceptT)

rule01 :: forall a. NumRule a
rule01 _ = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  a <- newTensor @a "a" [adim --> map]
  const1 <- newTensor @a "const1" [adim --> map]
  const2 <- newTensor @a "const2" [adim --> map]
  lhs <- clamp const1 (clamp const1 a const2) const2
  rhs <- clamp const1 a const2
  rewrite "Clamp(Const1,Clamp(Const1,A,Const2),Const2) ⇒ Clamp(Const1,A,Const2)" lhs rhs

rule02 :: forall a. NumRule a
rule02 _ = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  a <- newTensor @a "a" [adim --> map]
  const1 <- newTensor @a "const1" [adim --> map]
  const2 <- newTensor @a "const2" [adim --> map]
  lhs <- numBinOp Max const1 (numBinOp Min a const2)
  rhs <- clamp const1 a const2
  rewrite "Max(Broadcast(Const), Min(A, Broadcast(Const2))) ⇒ Clamp(A,Const,Const2)" lhs rhs

rule03 :: forall a. NumRule a
rule03 _ = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  a <- newTensor @a "a" [adim --> map]
  const1 <- newTensor @a "const1" [adim --> map]
  const2 <- newTensor @a "const2" [adim --> map]
  lhs <- numBinOp Min const1 (numBinOp Max a const2)
  rhs <- clamp const2 a const1
  rewrite "Min(Broadcast(Const), Max(A, Broadcast(Const2))) ⇒ Clamp(A,Const,Const2)" lhs rhs

main :: IO ()
main = do
  print "############################## rule01 ##############################"
  verifyNumDSL rule01
  print "############################## rule02 ##############################"
  verifyNumDSL rule02
  print "############################## rule03 ##############################"
  verifyNumDSL rule03
