module Main (main) where

import Grisette hiding ((-->))
import TensorRight

rule01 :: forall a. NumRule a
rule01 _ = do
  adim <- newAdim "adim"
  [sizeMap0, sizeMap1, startMap0] <- newMaps ["sizeMap0", "sizeMap1", "startMap0"] adim
  t1 <- newTensor @a "t1" [adim --> sizeMap0]
  t2 <- newTensor @a "t2" [adim --> sizeMap1]
  lhs <- numBinOp Add t1 (dynamicUpdateSlice (constant @a 0 [adim --> sizeMap0]) t2 [adim --> startMap0])
  rhs <- dynamicUpdateSlice t1 (numBinOp Add t2 (dynamicSlice t1 DySlice {start = [adim --> startMap0], sizes = [adim --> sizeMap1]})) [adim --> startMap0]
  rewrite "Add(A, DynamicUpdateSlice(Broadcast(0), B) ⇒ DynamicUpdateSlice(A,...)" lhs rhs

rule02 :: forall a. AnyDTypeRule a
rule02 _ = do
  adim <- newAdim "adim"
  [origSizeMap, newSizeMap, startMap, lowMap, interiorMap, highMap] <- newMaps ["origSizeMap", "newSizeMap", "startMap", "lowMap", "interiorMap", "highMap"] adim
  tensor <- newTensor @a "tensor" [adim --> origSizeMap]
  lhs <- dynamicUpdateSlice (constant @a "a" [adim --> newSizeMap]) tensor [adim --> startMap]
  rhs <- pad tensor ("a" :: a) [adim --> lowMap] [adim --> interiorMap] [adim --> highMap]
  precondition [lowMap] $ \[low] -> low .>= 0
  precondition [highMap] $ \[high] -> high .>= 0
  precondition [lowMap, startMap] $ \[low, start] -> low .== start
  precondition [interiorMap] $ \[interior] -> interior .== 0
  precondition [lowMap, highMap, origSizeMap, newSizeMap] $
    \[low, high, origSize, newSize] -> newSize .== origSize + low + high
  rewrite "DynamicUpdateSlice(Broadcast(Const),A,...) ⇒ Pad(" lhs rhs

rule03 :: forall a. AnyDTypeRule a
rule03 _ = do
  adim <- newAdim "adim"
  [sizeMap, startMap] <- newMaps ["sizeMap", "startMap"] adim
  t1 <- newTensor @a "t1" [adim --> sizeMap]
  t2 <- newTensor @a "t2" [adim --> sizeMap]
  lhs <- dynamicUpdateSlice t1 t2 [adim --> startMap]
  let rhs = t2
  precondition [startMap] $ \[start] -> start .== 0
  rewrite "DynamicUpdateSlice(A,...) ⇒ A" lhs rhs

rule04 :: forall a. AnyDTypeRule a
rule04 _ = do
  adim <- newAdim "adim"
  [sizeMap0, sizeMap1, startMap0, sliceSizeMap0, startMap1, startMap2] <- newMaps ["sizeMap0", "sizeMap1", "startMap0", "sliceSizeMap0", "startMap1", "startMap2"] adim
  t1 <- newTensor @a "t1" [adim --> sizeMap0]
  t2 <- newTensor @a "t2" [adim --> sizeMap1]
  lhs <- dynamicUpdateSlice t1 (dynamicUpdateSlice (dynamicSlice t1 DySlice {start = [adim --> startMap0], sizes = [adim --> sliceSizeMap0]}) t2 [adim --> startMap1]) [adim --> startMap0]
  rhs <- dynamicUpdateSlice t1 t2 [adim --> startMap2]
  precondition [startMap0, startMap1, startMap2] $
    \[start0, start1, start2] -> start2 .== start0 + start1
  precondition [startMap0] $ \[start0] -> start0 .>= 0
  precondition [startMap1] $ \[start1] -> start1 .>= 0
  precondition [sliceSizeMap0, startMap1, sizeMap1] $
    \[sliceSize0, start1, size1] -> start1 + size1 .<= sliceSize0
  precondition [startMap0, sliceSizeMap0, sizeMap0] $
    \[start0, sliceSize0, size0] -> start0 + sliceSize0 .<= size0
  rewrite "DynamicUpdateSlice(A, DynamicUpdateSlice(DynamicSlice(A,...), C ,...),...)) ⇒ DynamicUpdateSlice(A,C,...)" lhs rhs

main :: IO ()
main = do
  print "############################## rule01 ##############################"
  verifyNumDSL rule01
  print "############################## rule02 ##############################"
  verifyAnyDTypeDSL rule02
  print "############################## rule03 ##############################"
  verifyAnyDTypeDSL rule03
  print "############################## rule04 ##############################"
  verifyAnyDTypeDSL rule04
