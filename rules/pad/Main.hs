module Main (main) where

import Grisette hiding ((-->))
import TensorRight

rule01 :: forall a. AnyDTypeRule a
rule01 _ = do
  adim <- newAdim "adim"
  [sizeMap, lowMap, interiorMap, highMap] <- newMaps ["sizeMap", "lowMap", "interiorMap", "highMap"] adim
  tensor <- newTensor @a "tensor" [adim --> sizeMap]
  lhs <- pad tensor ("a" :: a) [adim --> lowMap] [adim --> interiorMap] [adim --> highMap]
  let rhs = tensor
  precondition [lowMap] $ \[low] -> low .== 0
  precondition [interiorMap] $ \[interior] -> interior .== 0
  precondition [highMap] $ \[high] -> high .== 0
  rewrite "Pad(A,val, 0_0_0) ⇒ A" lhs rhs

rule02 :: forall a. AnyDTypeRule a
rule02 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  [sizeMap0, lowMap0, interiorMap0, highMap0] <- newMaps ["sizeMap0", "lowMap0", "interiorMap0", "highMap0"] adim0
  [sizeMap1, lowMap1, interiorMap1lhs, interiorMap1rhs, highMap1] <- newMaps ["sizeMap1", "lowMap1", "interiorMap1lhs", "interiorMap1rhs", "highMap1"] adim1
  tensor <- newTensor @a "tensor" [adim0 --> sizeMap0, adim1 --> sizeMap1]
  lhs <- pad tensor ("a" :: a) [adim0 --> lowMap0, adim1 --> lowMap1] [adim0 --> interiorMap0, adim1 --> interiorMap1lhs] [adim0 --> highMap0, adim1 --> highMap1]
  rhs <- pad tensor ("a" :: a) [adim0 --> lowMap0, adim1 --> lowMap1] [adim0 --> interiorMap0, adim1 --> interiorMap1rhs] [adim0 --> highMap0, adim1 --> highMap1]
  precondition [interiorMap1lhs] $ \[interior1lhs] -> interior1lhs .>= 0
  precondition [interiorMap1rhs] $ \[interior1rhs] -> interior1rhs .== 0
  precondition [sizeMap1] $ \[size1] -> size1 .== 1
  rewrite "Pad(A,val, low_int_high) ⇒ Pad(A,val,low_0_high)" lhs rhs

rule03 :: forall a. AnyDTypeRule a
rule03 _ = do
  [adim0, adim1, adim2] <- newAdims ["adim0", "adim1", "adim2"]
  [sizeMap0, lowMap0, highMap0] <- newMaps ["sizeMap0", "lowMap0", "highMap0"] adim0
  [sizeMap1, lowMap1, highMap1] <- newMaps ["sizeMap1", "lowMap1", "highMap1"] adim1
  [sizeMap2, lowMap2, highMap2] <- newMaps ["sizeMap2", "lowMap2", "highMap2"] adim2
  intMap0 <- newConstMap "intMap0" 0 adim0
  intMap1 <- newConstMap "intMap1" 0 adim1
  intMap2 <- newConstMap "intMap2" 0 adim2

  tensor <- newTensor @a "tensor" [adim0 --> sizeMap0]

  lhs <- pad (broadcast tensor [adim1 --> sizeMap1, adim2 --> sizeMap2]) ("a" :: a) $
    Padding
      { low = [adim0 --> lowMap0, adim1 --> lowMap1, adim2 --> lowMap2],
        interior = [adim0 --> intMap0, adim1 --> intMap1, adim2 --> intMap2],
        high = [adim0 --> highMap0, adim1 --> highMap1, adim2 --> highMap2]
      }
  rhs <- broadcast (
          pad (broadcast tensor [adim1 --> sizeMap1]) ("a" :: a) $
            Padding
              { low = [adim0 --> lowMap0, adim1 --> lowMap1],
                interior = [adim0 --> intMap0, adim1 --> intMap1],
                high = [adim0 --> highMap0, adim1 --> highMap1]
              }) [adim2 --> sizeMap2]

  precondition [lowMap2] $ \[low2] -> low2 .== 0
  precondition [highMap2] $ \[high2] -> high2 .== 0

  rewrite "Pad(Broadcast(Const1), Const2, low_0_0) ⇒ Broadcast(Pad(Broadcast(Const1), Const2))" lhs rhs

rule04 :: forall a. AnyDTypeRule a
rule04 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  [sizeMap0, lowMap0, highMap0, startMap0, endMap0] <- newMaps ["sizeMap0", "lowMap0", "highMap0", "startMap0", "endMap0"] adim0
  [sizeMap1, lowMap1, highMap1, startMap1, endMap1] <- newMaps ["sizeMap1", "lowMap1", "highMap1", "startMap1", "endMap1"] adim1
  intMap0 <- newConstMap "intMap0" 0 adim0
  intMap1 <- newConstMap "intMap1" 0 adim1
  strideMap0 <- newConstMap "strideMap0" 1 adim0
  strideMap1 <- newConstMap "strideMap1" 1 adim1
  lowMapRhs <- newConstMap "lowMapRhs" 0 adim0
  highMapRhs <- newConstMap "highMapRhs" 0 adim0

  tensor <- newTensor @a "tensor" [adim0 --> sizeMap0, adim1 --> sizeMap1]

  lhs <- pad tensor ("a" :: a) $
    Padding
      { low = [adim0 --> lowMap0, adim1 --> lowMap1],
        interior = [adim0 --> intMap0, adim1 --> intMap1],
        high = [adim0 --> highMap0, adim1 --> highMap1]
      }
  rhs <- slice (
          pad tensor ("a" :: a) $
            Padding
              { low = [adim0 --> lowMapRhs, adim1 --> lowMap1],
                interior = [adim0 --> intMap0, adim1 --> intMap1],
                high = [adim0 --> highMapRhs, adim1 --> highMap1]
              }
          ) $ Slice
                { start = [adim0 --> startMap0, adim1 --> startMap1],
                  end = [adim0 --> endMap0, adim1 --> endMap1],
                  strides = [adim0 --> strideMap0, adim1 --> strideMap1]
                }
  
  precondition [lowMap0] $ \[low0] -> low0 .< 0
  precondition [highMap0] $ \[high0] -> high0 .< 0
  precondition [lowMap1] $ \[low1] -> low1 .> 0
  precondition [highMap1] $ \[high1] -> high1 .> 0
  precondition [startMap0, lowMap0] $ \[start0, low0] -> start0 .== abs low0
  precondition [sizeMap0, endMap0, highMap0] $ \[size0, end0, high0] -> end0 .== size0 + high0
  precondition [startMap1] $ \[start1] -> start1 .== 0
  precondition [sizeMap1, endMap1, lowMap1, highMap1] $ \[size1, end1, low1, high1] -> end1 .== size1 + low1 + high1

  rewrite "Pad(A, val, negative_negative) ⇒ Slice(Pad(A, val, 0_0), abs(negative),negative+size)" lhs rhs

main :: IO ()
main = do
  print "############################## rule01 ##############################"
  verifyAnyDTypeDSL rule01
  print "############################## rule02 ##############################"
  verifyAnyDTypeDSL rule02
  print "############################## rule03 ##############################"
  verifyAnyDTypeDSL rule03
  print "############################## rule04 ##############################"
  verifyAnyDTypeDSL rule04
