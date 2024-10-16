module Main (main) where

import Grisette hiding ((-->))
import TensorRight

rule01 :: forall a. AnyDTypeRule a
rule01 _ = do
  adim <- newAdim "adim"
  [sizeMap, startMap, endMap, strideMap] <-
    newMaps ["sizeMap", "startMap", "endMap", "strideMap"] adim
  tensor <- newTensor @a "tensor" [adim --> sizeMap]
  lhs <-
    slice tensor $
      Slice
        { start = [adim --> startMap],
          end = [adim --> endMap],
          strides = [adim --> strideMap]
        }
  let rhs = tensor
  precondition [startMap] $ \[start] -> start .== 0
  precondition [sizeMap, endMap] $ \[size, end] -> end .== size
  precondition [strideMap] $ \[stride] -> stride .== 1
  rewrite "Slice(A) ⇒ A" lhs rhs

rule02 :: forall a. AnyDTypeRule a
rule02 _ = do
  adim <- newAdim "adim"
  [origSizeMap, newSizeMap, startMap, endMap, strideMap] <- newMaps ["origSizeMap", "newSizeMap", "startMap", "endMap", "strideMap"] adim
  lhs <- slice (constant @a "a" [adim --> origSizeMap]) [adim --> startMap] [adim --> endMap] [adim --> strideMap]
  rhs <- constant @a "a" [adim --> newSizeMap]
  precondition [startMap] $ \[start] -> start .>= 0
  precondition [strideMap] $ \[stride] -> stride .>= 1
  precondition [newSizeMap, startMap, endMap, strideMap] $
    \[newSize, start, end, stride] -> newSize .== divOr 0 (end - start + stride - 1) stride
  precondition [origSizeMap, endMap] $ \[origSize, end] -> end .<= origSize
  rewrite "Slice(Const) ⇒ Const" lhs rhs

rule03 :: forall a. AnyDTypeRule a
rule03 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  [sizeMap0, startMap0, endMap0, strideMap0] <- newMaps ["sizeMap0", "startMap0", "endMap0", "strideMap0"] adim0
  [origSizeMap1, startMap1, endMap1, strideMap1] <- newMaps ["origSizeMap1", "startMap1", "endMap1", "strideMap1"] adim1
  newSizeMap1 <- newMap "newSizeMap1" adim1
  tensor <- newTensor @a "tensor" [adim0 --> sizeMap0]
  lhs <- slice (broadcast tensor [adim1 --> origSizeMap1]) [adim0 --> startMap0, adim1 --> startMap1] [adim0 --> endMap0, adim1 --> endMap1] [adim0 --> strideMap0, adim1 --> strideMap1]
  rhs <- broadcast (slice tensor [adim0 --> startMap0] [adim0 --> endMap0] [adim0 --> strideMap0]) [adim1 --> newSizeMap1]
  precondition [newSizeMap1, startMap1, endMap1, strideMap1] $
    \[newSize1, start1, end1, stride1] -> newSize1 .== divOr 0 (end1 - start1 + stride1 - 1) stride1
  rewrite "Slice(Broadcast(A)) ⇒ Broadcast(Slice(A))" lhs rhs
  
rule04 :: forall a. AnyDTypeRule a
rule04 _ = do
  adim <- newAdim "adim"
  [sizeMap, endMap, strideMap]
    <- newMaps ["sizeMap", "endMap", "strideMap"] adim
  [startMap, lowMap, highMap, intMap] <- newNonNegMaps ["startMap", "lowMap", "highMap", "intMap"] adim
  tensor <- newTensor @a "tensor" [adim --> sizeMap]

  lhs <- slice
    (
      pad tensor ("a" :: a) $
      Padding
        { low = [adim --> lowMap],
          interior = [adim --> intMap],
          high = [adim --> highMap]
        }
    ) $ Slice 
          { start = [adim --> startMap],
            end = [adim --> endMap],
            strides = [adim --> strideMap]
          }
  let rhs = tensor
  precondition [startMap, lowMap] $ \[start, low] -> start .== low
  precondition [strideMap, intMap] $ \[stride, int] -> stride .== int + 1
  precondition [sizeMap, startMap, endMap, strideMap] $
    \[size, start, end, stride] -> size .== divOr 0 (end - start + stride - 1) stride
  -- precondition [strideMap] $ \[stride] -> stride .>= 1
  precondition [sizeMap, endMap, lowMap, highMap, intMap] $
    \[size, end, low, high, int] -> end .<= size + low + high + (size - 1) * int
  rewrite "Slice(Pad(A)) ⇒ A" lhs rhs

rule05 :: forall a. AnyDTypeRule a
rule05 _ = do
  adim <- newAdim "adim"
  [sizeMap, endMap, strideMap, lowMap, highMap, broadcastMap]
    <- newMaps ["sizeMap", "endMap", "strideMap", "lowMap", "highMap", "broadcastMap"] adim
  [startMap, intMap] <- newNonNegMaps ["startMap", "intMap"] adim
  tensor <- newTensor @a "tensor" [adim --> sizeMap]

  lhs <- slice
    (
      pad tensor ("a" :: a) $
      Padding
        { low = [adim --> lowMap],
          interior = [adim --> intMap],
          high = [adim --> highMap]
        }
    ) $ Slice 
          { start = [adim --> startMap],
            end = [adim --> endMap],
            strides = [adim --> strideMap]
          }
  rhs <- constant @a "a" [adim --> broadcastMap]
  precondition [broadcastMap, startMap, endMap, strideMap] $
    \[broadcastSize, start, end, stride] -> broadcastSize .== divOr 0 (end - start + stride - 1) stride
  precondition [sizeMap, startMap, endMap, lowMap, intMap] $
    \[size, start, end, low, int] -> end .<= low .|| start .>= size + low + (size - 1) * int
  rewrite "Slice(Pad(A, v)) ⇒ Broadcast(v)" lhs rhs

rule06 :: forall a. AnyDTypeRule a
rule06 _ = do
  adim <- newAdim "adim"
  sizeMap <- newMap "sizeMap" adim
  [startMap, endMap, lowMap, highMap]
    <- newNonNegMaps ["startMap", "endMap", "lowMap", "highMap"] adim
  strideMap <- newConstMap "strideMap" 1 adim
  intMap <- newConstMap "intMap" 0 adim
  newStartMap <- combineMap "newStartMap" (\[a, b] -> a - b) [startMap, lowMap]
  newEndMap <- combineMap "newEndMap" (\[a, b] -> a - b) [endMap, lowMap]
  tensor <- newTensor @a "tensor" [adim --> sizeMap]

  lhs <- slice
    (
      pad tensor ("a" :: a) $
      Padding
        { low = [adim --> lowMap],
          interior = [adim --> intMap],
          high = [adim --> highMap]
        }
    ) $ Slice 
          { start = [adim --> startMap],
            end = [adim --> endMap],
            strides = [adim --> strideMap]
          }
  
  rhs <- slice tensor $ Slice 
          { start = [adim --> newStartMap],
            end = [adim --> newEndMap],
            strides = [adim --> strideMap]
          }
  precondition [startMap, lowMap] $ \[start, low] -> start .>= low
  precondition [sizeMap, endMap, lowMap, intMap] $
    \[size, end, low, int] -> end .<= size + low + (size - 1) * int
  rewrite "Slice(Pad(A, v)) ⇒ Slice(A)" lhs rhs

rule07 :: forall a. AnyDTypeRule a
rule07 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  [sizeMap0, endMap0, innerEnd0] <- newMaps ["sizeMap0", "endMap0", "innerEnd0"] adim0
  [startMap0, innerStart0] <- newNonNegMaps ["startMap0", "innerStart0"] adim0
  [sizeMap1, endMap1, innerEnd1] <- newMaps ["sizeMap1", "endMap1", "innerEnd1"] adim1
  [startMap1, innerStart1] <- newNonNegMaps ["startMap1", "innerStart1"] adim1
  rhsStart0 <- combineMap "rhsStart0" (\[a, b] -> a + b) [startMap0, innerStart0]
  rhsStart1 <- combineMap "rhsStart1" (\[a, b] -> a + b) [startMap1, innerStart1]
  rhsEnd0 <- combineMap "rhsEnd0" (\[a, b] -> a + b) [innerEnd0, startMap0]
  rhsEnd1 <- combineMap "rhsEnd1" (\[a, b] -> a + b) [innerEnd1, startMap1]
  strideMap0 <- newConstMap "strideMap0" 1 adim0
  strideMap1 <- newConstMap "strideMap1" 1 adim1

  tensor <- newTensor @a "tensor" [adim0 --> sizeMap0, adim1 --> sizeMap1]

  lhs <- slice
    (
      slice tensor $
        Slice
          { start = [adim0 --> startMap0, adim1 --> startMap1],
            end = [adim0 --> endMap0, adim1 --> endMap1],
            strides = [adim0 --> strideMap0, adim1 --> strideMap1]
          }
    ) $ Slice
          {
            start = [adim0 --> innerStart0, adim1 --> innerStart1],
            end = [adim0 --> innerEnd0, adim1 --> innerEnd1],
            strides = [adim0 --> strideMap0, adim1 --> strideMap1]
          }
  rhs <- slice tensor $ Slice
          {
            start = [adim0 --> rhsStart0, adim1 --> rhsStart1],
            end = [adim0 --> rhsEnd0, adim1 --> rhsEnd1],
            strides = [adim0 --> strideMap0, adim1 --> strideMap1]
          }
  
  rewrite "Slice(Slice(A)) ⇒ Slice(A)" lhs rhs

rule08 :: forall a. AnyDTypeRule a
rule08 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  [sizeMapA0, sizeMapB0, endMap0, strideMap0] <- newMaps ["sizeMapA0", "sizeMapB0", "endMap0", "strideMap0"] adim0
  startMap0 <- newNonNegMap "startMap0" adim0
  [sizeMapA1, sizeMapB1, endMap1, strideMap1] <- newMaps ["sizeMapA1", "sizeMapB1", "endMap1", "strideMap1"] adim1
  startMap1 <- newNonNegMap "startMap1" adim1
  tensorA <- newTensor @a "tensorA" [adim0 --> sizeMapA0, adim1 --> sizeMapA1]
  tensorB <- newTensor @a "tensorB" [adim0 --> sizeMapB0, adim1 --> sizeMapB1]

  lhs <- slice (concatTensor tensorA tensorB (ByAdim adim0)) $
          Slice
            {
              start = [adim0 --> startMap0, adim1 --> startMap1],
              end = [adim0 --> endMap0, adim1 --> endMap1],
              strides = [adim0 --> strideMap0, adim1 --> strideMap1]
            }
  let rhs = tensorB

  precondition [startMap1] $ \[start1] -> start1 .== 0
  precondition [sizeMapA1, endMap1] $ \[sizeA1, end1] -> end1 .== sizeA1
  precondition [strideMap0] $ \[stride0] -> stride0 .== 1
  precondition [strideMap1] $ \[stride1] -> stride1 .== 1
  precondition [sizeMapA0, startMap0] $ \[sizeA0, start0] -> start0 .== sizeA0
  precondition [sizeMapA0, sizeMapB0, endMap0] $
    \[sizeA0, sizeB0, end0] -> end0 .== sizeA0 + sizeB0

  rewrite "Slice(Concat(A,B), A.size, A.size+B.size) ⇒ B" lhs rhs

rule09 :: forall a. AnyDTypeRule a
rule09 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  [sizeMap0, startMap0, endMap0, strideMap0]
    <- newMaps ["sizeMap0", "startMap0", "endMap0", "strideMap0"] adim0
  [sizeMap1, startMap1, endMap1, strideMap1, broadcastSize1]
    <- newMaps ["sizeMap1", "startMap1", "endMap1", "strideMap1", "broadcastSize1"] adim1
  tensor <- newTensor @a "tensor" [adim0 --> sizeMap0]

  lhs <- slice (broadcast tensor [adim1 --> sizeMap1]) $
          Slice
            {
              start = [adim0 --> startMap0, adim1 --> startMap1],
              end = [adim0 --> endMap0, adim1 --> endMap1],
              strides = [adim0 --> strideMap0, adim1 --> strideMap1]
            }
  rhs <- broadcast tensor [adim1 --> broadcastSize1]

  precondition [broadcastSize1] $ \[s] -> s .> 0
  precondition [sizeMap1] $ \[s] -> s .> 0
  precondition [startMap0] $ \[s] -> s .== 0
  precondition [endMap0, sizeMap0] $ \[e, s] -> e .== s
  precondition [strideMap0] $ \[p] -> p .== 1
  precondition [broadcastSize1, startMap1, endMap1, strideMap1] $
    \[b, s, e, p] ->
        symIte (modOr 0 (e - s) p .== 0)
               (divOr 0 (e - s) p)
               (divOr 0 (e - s) p + 1) .== b 

  rewrite "Slice(Broadcast(A)) ⇒ Broadcast(A)" lhs rhs

rule10 :: forall a. AnyDTypeRule a
rule10 _ = do
  adim <- newAdim "adim"
  [sizeMapA, sizeMapB, endMap] <- newMaps ["sizeMapA", "sizeMapB", "endMap"] adim
  startMap <- newNonNegMap "startMap" adim
  strideMap <- newConstMap "strideMap" 1 adim
  tensorA <- newTensor @a "tensorA" [adim --> sizeMapA]
  tensorB <- newTensor @a "tensorB" [adim --> sizeMapB]

  lhs <- slice (concatTensor tensorA tensorB (ByAdim adim)) $
          Slice
            {
              start = [adim --> startMap],
              end = [adim --> endMap],
              strides = [adim --> strideMap]
            }
  rhs <- slice tensorA $
          Slice
            {
              start = [adim --> startMap],
              end = [adim --> endMap],
              strides = [adim --> strideMap]
            }
  
  precondition [sizeMapA, endMap] $
    \[sizeA, end] -> end .<= sizeA

  rewrite "Slice(Concat(A,B)) ⇒ Slice(A)" lhs rhs

rule11 :: forall a. AnyDTypeRule a
rule11 _ = do
  adim <- newAdim "adim"
  [sizeMapA, sizeMapB, endMap] <- newMaps ["sizeMapA", "sizeMapB", "endMap"] adim
  startMap <- newNonNegMap "startMap" adim
  startMapB <- combineMap "startMapB" (\[a, b] -> a - b) [startMap, sizeMapA]
  endMapB <- combineMap "endMapB" (\[a, b] -> a - b) [endMap, sizeMapA]
  strideMap <- newConstMap "strideMap" 1 adim
  tensorA <- newTensor @a "tensorA" [adim --> sizeMapA]
  tensorB <- newTensor @a "tensorB" [adim --> sizeMapB]

  lhs <- slice (concatTensor tensorA tensorB (ByAdim adim)) $
          Slice
            {
              start = [adim --> startMap],
              end = [adim --> endMap],
              strides = [adim --> strideMap]
            }
  rhs <- slice tensorB $
          Slice
            {
              start = [adim --> startMapB],
              end = [adim --> endMapB],
              strides = [adim --> strideMap]
            }
  
  precondition [sizeMapA, startMap] $
    \[sizeA, start] -> start .> sizeA

  rewrite "Slice(Concat(A,B)) ⇒ Slice(B)" lhs rhs

rule12 :: forall a. AnyDTypeRule a
rule12 _ = do
  adim <- newAdim "adim"
  [sizeMapA, sizeMapB, sizeMapC, startMap, endMap, strideMap]
    <- newMaps ["sizeMapA", "sizeMapB", "sizeMapC", "startMap", "endMap", "strideMap"] adim
  startMapB <- combineMap "startMapB" (\[a, b] -> a - b) [startMap, sizeMapA]
  endMapB <- combineMap "endMapB" (\[a, b] -> a - b) [endMap, sizeMapA]

  tensorA <- newTensor @a "tensorA" [adim --> sizeMapA]
  tensorB <- newTensor @a "tensorB" [adim --> sizeMapB]
  tensorC <- newTensor @a "tensorC" [adim --> sizeMapC]

  lhs <- slice (concatTensorList [tensorA, tensorB, tensorC] (ByAdim adim)) $
          Slice
            {
              start = [adim --> startMap],
              end = [adim --> endMap],
              strides = [adim --> strideMap]
            }
  rhs <- slice (concatTensor tensorB tensorC (ByAdim adim)) $
          Slice
            {
              start = [adim --> startMapB],
              end = [adim --> endMapB],
              strides = [adim --> strideMap]
            }
  
  precondition [sizeMapA, startMap] $ \[sizeA, start] -> sizeA .<= start

  rewrite "Slice(Concat(A,B,C)) ⇒ Slice(Concat(B,C))" lhs rhs

rule13 :: forall a. AnyDTypeRule a
rule13 _ = do
  adim <- newAdim "adim"
  [sizeMap, startMapLhs, endMapLhs, strideMap, startMapRhs, endMapRhs]
    <- newMaps ["sizeMap", "startMapLhs", "endMapLhs", "strideMap", "startMapRhs", "endMapRhs"] adim
  tensor <- newTensor @a "tensor" [adim --> sizeMap]

  lhs <- slice (reverseTensor tensor [ByAdim adim]) $
          Slice
            {
              start = [adim --> startMapLhs],
              end = [adim --> endMapLhs],
              strides = [adim --> strideMap]
            }
  rhs <- reverseTensor (slice tensor $
          Slice
            {
              start = [adim --> startMapRhs],
              end = [adim --> endMapRhs],
              strides = [adim --> strideMap]
            }) [ByAdim adim]
  
  let find_nth start end = divOr 0 (end - start)
  let count start end stride =
        symIte (modOr 0 (end - start) stride .== 0)
               (find_nth start end stride - 1)
               (find_nth start end stride)
  let new_start size start end stride = start + stride * count start end stride + 1 - size
  let new_end size start end stride = 
        symIte (size .>= new_start size start end stride + end - start)
               (new_start size start end stride + end - start)
               size
                            
  precondition [sizeMap, startMapLhs, endMapLhs, strideMap, startMapRhs] $
    \[size, startLhs, endLhs, stride, startRhs] -> startRhs .== new_start size startLhs endLhs stride
  precondition [sizeMap, startMapLhs, endMapLhs, strideMap, endMapRhs] $
    \[size, startLhs, endLhs, stride, endRhs] -> endRhs .== new_end size startLhs endLhs stride

  rewrite "Slice(Reverse(A,dims),start,stride,end) ⇒ Reverse(Slice(A,...),dims)" lhs rhs

main :: IO ()
main = do
  print "############################## rule01 ##############################"
  verifyAnyDTypeDSL rule01
  print "############################## rule02 ##############################"
  verifyAnyDTypeDSL rule02
  print "############################## rule03 ##############################"
  verifyAnyDTypeDSL rule03
  print "############################## rule04 ##############################"
  verifyAnyDTypeDSLWith (withTimeout 10000000 cvc5) rule04
  print "############################## rule05 ##############################"
  verifyAnyDTypeDSL rule05
  print "############################## rule06 ##############################"
  verifyAnyDTypeDSL rule06
  print "############################## rule07 ##############################"
  verifyAnyDTypeDSL rule07
  print "############################## rule08 ##############################"
  verifyAnyDTypeDSL rule08
  print "############################## rule09 ##############################"
  verifyAnyDTypeDSL rule09
  print "############################## rule10 ##############################"
  verifyAnyDTypeDSL rule10
  print "############################## rule11 ##############################"
  verifyAnyDTypeDSL rule11
  print "############################## rule12 ##############################"
  verifyAnyDTypeDSL rule12
  print "############################## rule13 ##############################"
  verifyAnyDTypeDSLWith (withTimeout 10000000 z3) rule13