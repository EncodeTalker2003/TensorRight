module Main (main) where

import Debug.Trace (traceShow)
import Grisette hiding (dot, (-->))
import TensorRight

rule01 :: forall a. NumRule a
rule01 _ = do
  [adim0, adim1, adim2] <- newAdims ["adim0", "adim1", "adim2"]
  [xs0, ys0, dotsi, sixc, siyd, sir, adim0AllOne] <-
    newMaps
      ["xs0", "ys0", "dotsi", "sixc", "siyd", "sir", "allOne"]
      adim0
  [xs1, ys1] <- newMaps ["xs1", "ys1"] adim1
  [cs2, ds2] <- newMaps ["cs2", "ds2"] adim2
  x <- newTensor @a "x" [adim0 --> xs0, adim1 --> xs1]
  y <- newTensor @a "y" [adim0 --> ys0, adim1 --> ys1]
  c <- newTensor @a "c" [adim0 --> xs0, adim2 --> cs2]
  d <- newTensor @a "d" [adim0 --> ys0, adim2 --> ds2]
  lhs <-
    dot
      (concatTensor x y $ ByAdim adim0)
      (concatTensor c d $ ByAdim adim0)
      [adim0 --> dotsi]
      []
  rhs <-
    reduce
      ( concatTensor
          (broadcast (dot x c [adim0 --> sixc] []) [adim0 --> adim0AllOne])
          (broadcast (dot y d [adim0 --> siyd] []) [adim0 --> adim0AllOne])
          $ ByAdim adim0
      )
      [adim0 --> sir]
  precondition [adim0AllOne] $ \[adim0AllOne] -> adim0AllOne .== 1
  let siCondition [vdotsi, vsixc, vsiyd, vsir, vxs0, vys0] =
        symIte
          (vsir .== 0)
          (vdotsi .== vsixc)
          (vdotsi .== vsiyd + vxs0)
          .&& (vsixc .>= 0)
          .&& (vsiyd .>= 0)
          .&& (vsixc .< vxs0)
          .&& (vsiyd .< vys0)
          .&& (vsir .== 0 .|| vsir .== 1)
      siCondition _ = undefined
  siRelation [dotsi, sixc, siyd, sir, xs0, ys0] siCondition
  checkSIMap [dotsi] [sir, sixc, siyd]
  let lhsStr = "Dot(Concat(X, Y), Concat(C, D))"
  let rhsStr = "Reduce(Concat(Broadcast(Dot(X, C)), Broadcast(Dot(Y, D)))"
  rewrite (lhsStr <> " ⇒ " <> rhsStr) lhs rhs

rule02 :: forall a. NumRule a
rule02 _ = do
  [adim0, adim1, adim2, adim3] <- newAdims ["adim0", "adim1", "adim2", "adim3"]
  [size0, lhssi0, rhssi0] <- newMaps ["size0", "lhssi0", "rhssi0"] adim0
  size1 <- newMap "size1" adim1
  size2 <- newMap "size2" adim2
  size3 <- newMap "size3" adim3
  a <- newTensor @a "a" [adim0 --> size0, adim1 --> size1, adim2 --> size2]
  b <- newTensor @a "b" [adim0 --> size0, adim1 --> size1, adim3 --> size3]
  lhs <- dot a b [adim0 --> lhssi0] [ByAdim adim1]
  rhs <- dot b a [adim0 --> rhssi0] [ByAdim adim1]
  siRelation [lhssi0, rhssi0] $ \[vlhssi0, vrhssi0] -> vlhssi0 .== vrhssi0
  checkSIMap [lhssi0] [rhssi0]
  rewrite "Dot(A, B) ⇒ Dot(B, A)" lhs rhs

rule03 :: forall a. NumRule a
rule03 _ = do
  [adim0, adim1, adim2, adim3] <- newAdims ["adim0", "adim1", "adim2", "adim3"]
  [sizea0] <- newMaps ["sizea0"] adim0
  [sizea1, sizeb1, siabLhs, siabRhs] <- newMaps ["sizea1", "sizeb1", "siabLhs", "siabRhs"] adim1
  [sizeb2, sizec2, sibcLhs, sibcRhs] <- newMaps ["sizeb2", "sizec2", "sibcLhs", "sibcRhs"] adim2
  [sizec3] <- newMaps ["sizec3"] adim3

  tensorA <- newTensor @a "tensorA" [adim0 --> sizea0, adim1 --> sizea1]
  tensorB <- newTensor @a "tensorB" [adim1 --> sizeb1, adim2 --> sizeb2]
  tensorC <- newTensor @a "tensorC" [adim2 --> sizec2, adim3 --> sizec3]

  lhs <-
    dot
      tensorA
      ( dot
          tensorB
          tensorC
          [adim2 --> sibcLhs]
          []
      )
      [adim1 --> siabLhs]
      []
  rhs <-
    dot
      ( dot
          tensorA
          tensorB
          [adim1 --> siabRhs]
          []
      )
      tensorC
      [adim2 --> sibcRhs]
      []

  siRelation [siabLhs, siabRhs] $ \[vsiabLhs, vsiabRhs] -> vsiabLhs .== vsiabRhs
  siRelation [sibcLhs, sibcRhs] $ \[vsibcLhs, vsibcRhs] -> vsibcLhs .== vsibcRhs
  checkSIMap [siabLhs, sibcLhs] [siabRhs, sibcRhs]

  rewrite "Dot(A,Dot(B,C)) ⇒ Dot(Dot(A,B),C)" lhs lhs

rule04 :: forall a. NumRule a
rule04 _ = do
  [adim0, adim1, adim2] <- newAdims ["adim0", "adim1", "adim2"]
  adim0Size <- newMap "adim0Size" adim0
  adim1Size <- newMap "adim1Size" adim1
  adim2Size <- newMap "adim2Size" adim2
  x <- newTensor @a "x" [adim0 --> adim0Size, adim1 --> adim1Size]
  y <- newTensor @a "y" [adim0 --> adim0Size, adim2 --> adim2Size]
  lhs <- dot x y [] [ByAdim adim0]
  rhs <-
    numBinOp
      Mul
      (broadcast x [adim2 --> adim2Size])
      (broadcast y [adim1 --> adim1Size])
  rewrite "Dot(A,B) ⇒ Mul(Broadcast(A), Broadcast(B)) when no contraction" lhs rhs

rule05 :: forall a. NumRule a
rule05 _ = do
  [adim0, adim1, cadim0, cadim1, badim0, badim1] <-
    newAdims ["adim0", "adim1", "cadim0", "cadim1", "badim0", "badim1"]
  adim0Size <- newMap "adim0Size" adim0
  adim1Size <- newMap "adim1Size" adim1
  cadim0Size <- newMap "cadim0Size" cadim0
  cadim1Size <- newMap "cadim1Size" cadim1
  badim0Size <- newMap "badimSize" badim0
  badim1Size <- newMap "badimSize" badim1
  si0 <- newMap "si0" cadim0
  si1 <- newMap "si1" cadim1
  x <-
    newTensor @a
      "x"
      [ adim0 --> adim0Size,
        cadim0 --> cadim0Size,
        cadim1 --> cadim1Size,
        badim0 --> badim0Size,
        badim1 --> badim1Size
      ]
  y <-
    newTensor @a
      "y"
      [ adim1 --> adim1Size,
        cadim0 --> cadim0Size,
        cadim1 --> cadim1Size,
        badim0 --> badim0Size,
        badim1 --> badim1Size
      ]
  lhs <- dot x y [cadim0 --> si0, cadim1 --> si1] [ByAdim badim0, ByAdim badim1]
  rhs <-
    constant @a
      0
      [ adim0 --> adim0Size,
        adim1 --> adim1Size,
        badim0 --> badim0Size,
        badim1 --> badim1Size
      ]
  precondition'
    [ adim0Size,
      adim1Size,
      cadim0Size,
      cadim1Size,
      badim0Size,
      badim1Size
    ]
    $ \[adim0Size, adim1Size, cadim0Size, cadim1Size, badim0Size, badim1Size] ->
      zipCondition (\[adim0Size] -> adim0Size .== 0) [adim0Size]
        .|| zipCondition (\[adim1Size] -> adim1Size .== 0) [adim1Size]
        .|| zipCondition (\[cadimSize] -> cadimSize .== 0) [cadim0Size]
        .|| zipCondition (\[cadimSize] -> cadimSize .== 0) [cadim1Size]
        .|| zipCondition (\[badimSize] -> badimSize .== 0) [badim0Size]
        .|| zipCondition (\[badimSize] -> badimSize .== 0) [badim1Size]
  rewrite "Dot(A,B) ⇒ 0 when one of the dimensions is 0" lhs rhs

rule06 :: forall a. NumRule a
rule06 _ = do
  [adim, cadim, badim] <- newAdims ["adim", "cadim", "badim"]
  adimSize <- newMap "adimSize" adim
  cadimSize <- newMap "cadimSize" cadim
  badimSize <- newMap "badimSize" badim
  x <- newTensor @a "x" [adim --> adimSize, cadim --> cadimSize, badim --> badimSize]
  y <- newTensor @a "y" [cadim --> cadimSize, badim --> badimSize]
  si <- newMap "si" cadim
  rsi <- newMap "rsi" cadim
  lhs <- dot x y [cadim --> si] [ByAdim badim]
  rhs <-
    reduce
      ( numBinOp
          Mul
          x
          (broadcast y [adim --> adimSize])
      )
      [cadim --> rsi]
  siRelation [si, rsi] $ \[vsi, vrsi] -> vsi .== vrsi
  checkSIMap [si] [rsi]
  rewrite
    ( "Dot(A,B) ⇒ Reduce(Mul(Broadcast(Transpose(A)), Broadcast(Transpose(B)))) "
        <> "when rhs only have contraction and batch adims"
    )
    lhs
    rhs

main :: IO ()
main = do
  print "############################## rule01 ##############################"
  verifyNumDSLWith cvc5 rule01
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
