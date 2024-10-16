module Main (main) where

import Grisette hiding ((-->))
import TensorRight

rule01 :: DSLContext Rewrite
rule01 = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  input <- newTensor @SymBool "input" [adim --> map]
  lhs <- boolUnaryOp Not (boolUnaryOp Not input)
  let rhs = input
  rewrite "Not(Not(A)) ⇒ A" lhs rhs

rule02 :: forall a. NumRule a
rule02 _ = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  input <- newTensor @a "input" [adim --> map]
  lhs <- numUnaryOp Neg (numUnaryOp Neg input)
  let rhs = input
  rewrite "Negate(Negate(A)) ⇒ A" lhs rhs

main :: IO ()
main = do
  print "############################## rule01 ##############################"
  verifyDSL rule01
  print "############################## rule02 ##############################"
  verifyNumDSL rule02
