module Main (main) where

import Control.Monad.Except (runExceptT)
import Grisette hiding ((-->))
import TensorRight
import TensorRight.Internal.Core.Tensor.TensorInt (TensorDivMod (tensorDiv))

rule00 :: forall a. NumRule a
rule00 _ = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  tensor <- newTensor @a "tensor" [adim --> map]
  constTensor <- constant @a 1 [adim --> map]
  lhs <- numBinOp Div tensor constTensor
  let rhs = tensor
  rewrite "Div(A,1) ⇒ A" lhs rhs

rule01 :: DSLContext Rewrite
rule01 = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  tensor <- newTensor @TensorReal "tensor" [adim --> map]
  let c = "c" :: TensorReal
  constTensor <- constant @TensorReal c [adim --> map]
  let creci = tensorDiv 1 "c" :: TensorReal
  constTensorreci <- constant @TensorReal creci [adim --> map]
  lhs <- numBinOp Div tensor constTensor
  rhs <- numBinOp Mul tensor constTensorreci
  rewrite "Div(A,Const) ⇒ Mul(A,1/Const)" lhs rhs

rule02 :: DSLContext Rewrite
rule02 = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  a <- newTensor @TensorReal "a" [adim --> map]
  b <- newTensor @TensorReal "b" [adim --> map]
  c <- newTensor @TensorReal "c" [adim --> map]
  d <- newTensor @TensorReal "d" [adim --> map]
  lhs <- numBinOp Div (numBinOp Div a b) (numBinOp Div c d)
  rhs <- numBinOp Div (numBinOp Mul a d) (numBinOp Mul b c)
  rewrite "Divide(Divide(A,B), Divide(C,D)) ⇒ Divide(Mul(A,D), Mul(B,C))" lhs rhs

rule03 :: DSLContext Rewrite
rule03 = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  a <- newTensor @TensorReal "a" [adim --> map]
  b <- newTensor @TensorReal "b" [adim --> map]
  c <- newTensor @TensorReal "c" [adim --> map]
  lhs <- numBinOp Div a (numBinOp Div b c)
  rhs <- numBinOp Div (numBinOp Mul a c) b
  rewrite "Divide(A, Divide(B,C)) ⇒ Divide(Mul(A,C), B)" lhs rhs

rule04 :: DSLContext Rewrite
rule04 = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  a <- newTensor @TensorInt "a" [adim --> map]
  b <- newTensor @TensorInt "b" [adim --> map]
  lhs <- numBinOp Rem (numBinOp Rem a b) b
  rhs <- numBinOp Rem a b
  rewrite "Rem(Rem(A,B),B) ⇒ Rem(A,B)" lhs rhs

rule05 :: DSLContext Rewrite
rule05 = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  const <- constant @TensorInt "c" [adim --> map]
  lhs <- numBinOp Rem (iota [adim --> map] (ByAdim adim)) const
  rhs <- iota [adim --> map] (ByAdim adim)
  precondition [map] $ \[m] -> m .<= "c"
  rewrite "Rem(Iota,Const) ⇒ Iota" lhs rhs

rule06 :: DSLContext Rewrite
rule06 = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  let c = "c" :: SymInteger
  const <- constant @TensorInt (nonInf c) [adim --> map]
  lhs <- numBinOp Rem (numBinOp Add (iota [adim --> map] (ByAdim adim)) const) const
  rhs <- numBinOp Rem (iota [adim --> map] (ByAdim adim)) const
  precondition [map] $ \[m] -> c .>= 0
  rewrite "Rem(Add(Iota,Const), Const) ⇒ Rem(Iota,Const)" lhs rhs

rule07 :: DSLContext Rewrite
rule07 = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  let xv = "x" :: SymInteger
  let nv = "n" :: SymInteger
  x <- constant @TensorInt (nonInf xv) [adim --> map]
  n <- constant @TensorInt (nonInf nv) [adim --> map]
  lhs <- numBinOp Rem (numBinOp Add x n) n
  rhs <- numBinOp Rem x n
  precondition [map] $ \[m] ->
    symIte (xv .>= 0) (xv + nv .>= 0) (xv + nv .< 0)
  rewrite "Rem(Add(X,Const), Const) ⇒ Rem(X,Const)" lhs rhs

main :: IO ()
main = do
  verifyNumDSL rule00
  verifyDSL rule01
  verifyDSL rule02
  verifyDSL rule03
  verifyDSL rule04
  verifyDSL rule05
  verifyDSLWith (withTimeout 10000000 z3) rule06
  verifyDSLWith (withTimeout 10000000 z3) rule07
