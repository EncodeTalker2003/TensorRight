module Main (main) where

import Grisette hiding ((-->))
import TensorRight

rule01 :: forall a. AnyDTypeRule a
rule01 _ = do
  adim <- newAdim "adim"
  [adimSize, adimStart, adimLength, adimEnd, adimStride] <-
    newMaps ["adimSize", "adimStart", "adimLength", "adimEnd", "adimStride"] adim

  tensor <- newTensor @a "tensor" [adim --> adimSize]

  lhs <-
    dynamicSlice tensor $
      DySlice {start = [adim --> adimStart], sizes = [adim --> adimLength]}
  rhs <-
    slice tensor $
      Slice
        { start = [adim --> adimStart],
          end = [adim --> adimEnd],
          strides = [adim --> adimStride]
        }

  precondition [adimStride] $ \[adimStride] -> adimStride .== 1
  precondition [adimEnd, adimStart, adimLength] $
    \[adimEnd, adimStart, adimLength] -> adimEnd .== adimStart + adimLength

  rewrite "DynamicSlice(A) ⇒ Slice(A)" lhs rhs

rule02 :: forall a. AnyDTypeRule a
rule02 _ = do
  adim <- newAdim "adim"
  [sizeMap, startMap, sliceSizeMap] <- newMaps ["sizeMap", "startMap", "sliceSizeMap"] adim
  tensor <- newTensor @a "tensor" [adim --> sizeMap]
  lhs <- dynamicSlice tensor DySlice {start = [adim --> startMap], sizes = [adim --> sliceSizeMap]}
  let rhs = tensor
  precondition [startMap] $ \[start] -> start .== 0
  precondition [sliceSizeMap, sizeMap] $ \[sliceSize, size] -> sliceSize .== size
  rewrite "DynamicSlice(A,...) ⇒ A // output shape is the same as input shape" lhs rhs

rule03 :: forall a. AnyDTypeRule a
rule03 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  [sizeMap0, startMap0, sliceSizeMap0] <- newMaps ["sizeMap0", "startMap0", "sliceSizeMap0"] adim0
  [sizeMap1, startMap1, sliceSizeMap1] <- newMaps ["sizeMap1", "startMap1", "sliceSizeMap1"] adim1
  tensor <- newTensor @a "tensor" [adim0 --> sizeMap0]
  lhs <- dynamicSlice (broadcast tensor [adim1 --> sizeMap1]) DySlice {start = [adim0 --> startMap0, adim1 --> startMap1], sizes = [adim0 --> sliceSizeMap0, adim1 --> sliceSizeMap1]}
  rhs <- broadcast (dynamicSlice tensor DySlice {start = [adim0 --> startMap0], sizes = [adim0 --> sliceSizeMap0]}) [adim1 --> sliceSizeMap1]
  precondition [startMap1] $
    \[start1] -> start1 .>= 0
  precondition [sizeMap1, startMap1, sliceSizeMap1] $
    \[size1, start1, sliceSize1] -> start1 + sliceSize1 .<= size1
  rewrite "DynamicSlice(Broadcast(A),...) ⇒ Broadcast(DynamicSlice(A,...))" lhs rhs

rule04 :: forall a. AnyDTypeRule a
rule04 _ = do
  adim <- newAdim "adim"
  [sizeMap, startMap, sliceSizeMap] <- newMaps ["sizeMap", "startMap", "sliceSizeMap"] adim
  tensor <- newTensor @a "tensor" [adim --> sizeMap @@ "label1"]
  lhs <- relabel (dynamicSlice tensor DySlice {start = [ByLabel "label1" --> startMap], sizes = [ByLabel "label1" --> sliceSizeMap]}) [ByLabel "label1" --> ByLabel "label2"]
  rhs <- dynamicSlice (relabel tensor [ByLabel "label1" --> ByLabel "label2"]) DySlice {start = [ByLabel "label2" --> startMap], sizes = [ByLabel "label2" --> sliceSizeMap]}
  rewrite "DynamicSlice(Transpose(A),...) ⇒ Transpose(DynamicSlice(A,...))" lhs rhs

rule05 :: forall a. AnyDTypeRule a
rule05 _ = do
  adim <- newAdim "adim"
  [sizeMap, startMap1, startMap2, startMap3, sliceSizeMap1, sliceSizeMap2, sliceSizeMap3] <- newMaps ["sizeMap", "startMap1", "startMap2", "startMap3", "sliceSizeMap1", "sliceSizeMap2", "sliceSizeMap3"] adim
  tensor <- newTensor @a "tensor" [adim --> sizeMap]
  lhs <- dynamicSlice (dynamicSlice tensor DySlice {start = [adim --> startMap1], sizes = [adim --> sliceSizeMap1]}) DySlice {start = [adim --> startMap2], sizes = [adim --> sliceSizeMap2]}
  rhs <- dynamicSlice tensor DySlice {start = [adim --> startMap3], sizes = [adim --> sliceSizeMap3]}
  precondition [startMap1, startMap2, startMap3] $
    \[start1, start2, start3] -> start3 .== start1 + start2
  precondition [sliceSizeMap2, sliceSizeMap3] $
    \[sliceSize2, sliceSize3] -> sliceSize3 .== sliceSize2
  precondition [startMap1] $ \[start1] -> start1 .>= 0
  precondition [startMap2] $ \[start2] -> start2 .>= 0
  precondition [sizeMap, startMap1, sliceSizeMap1] $
    \[size, start1, sliceSize1] -> start1 + sliceSize1 .<= size
  precondition [sliceSizeMap1, startMap2, sliceSizeMap2] $
    \[sliceSize1, start2, sliceSize2] -> start2 + sliceSize2 .<= sliceSize1
  rewrite "DynamicSlice(DynamicSlice(A,...),...) ⇒ DynamicSlice(A,...)" lhs rhs

rule06 :: DSLContext Rewrite
rule06 = do
  adim <- newAdim "adim"
  [sizeMap, startMap, lengthMap] <- newMaps ["sizeMap", "startMap", "lengthMap"] adim

  lhs <- dynamicSlice (iota [adim --> sizeMap] (ByAdim adim)) $
          DySlice
          {
            start = [adim --> startMap],
            sizes = [adim --> lengthMap]
          }
  rhsSize <- newConstMap "size" 1 adim
  -- Cannot express rule since we need a precondition on "a"
  rhs <- constant @TensorInt "a" [adim --> rhsSize]
  rewrite "DynamicSlice(Iota) ⇒ index" lhs rhs

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
  print "############################## rule05 ##############################"
  verifyAnyDTypeDSL rule05
