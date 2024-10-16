module Main (main) where

import TensorRight

rule01 :: DSLContext Rewrite
rule01 = do
  adim <- newAdim "adim"
  size <- newConstMap "size" 1 adim

  lhs <- iota [adim --> size] (ByAdim adim)
  rhs <- constant @TensorInt 0 [adim --> size]
  rewrite "Iota ⇒ Zero" lhs rhs

main :: IO ()
main = do
  print "############################## rule01 ##############################"
  verifyDSL rule01

