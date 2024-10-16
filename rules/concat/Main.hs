module Main (main) where

import Grisette hiding ((-->))
import TensorRight

rule01 :: forall a. AnyDTypeRule a
rule01 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  adim0size <- newMap "adim0size" adim0
  [adim1size0, adim1size1] <- newMaps ["adim1size0", "adim1size1"] adim1
  t1 <- newTensor @a "t1" [adim0 --> adim0size, adim1 --> adim1size0 @@ "label1"]
  t2 <- newTensor @a "t2" [adim0 --> adim0size, adim1 --> adim1size1 @@ "label1"]
  lhs <- concatTensorList [t1, t2] (ByLabel "label1")
  rhs <- concatTensor t1 t2 (ByLabel "label1")
  rewrite "ConcatList(A, B) ⇒ Concat(A, B)" lhs rhs

rule02 :: forall a. AnyDTypeRule a
rule02 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  adim0size <- newMap "adim0size" adim0
  [adim1size0, adim1size1, adim1size2] <- newMaps ["adim1size0", "adim1size1", "adim1size2"] adim1
  t1 <- newTensor @a "t1" [adim0 --> adim0size, adim1 --> adim1size0 @@ "label1"]
  t2 <- newTensor @a "t2" [adim0 --> adim0size, adim1 --> adim1size1 @@ "label1"]
  t3 <- newTensor @a "t3" [adim0 --> adim0size, adim1 --> adim1size2 @@ "label1"]
  lhs <- concatTensor t1 (concatTensor t2 t3 (ByLabel "label1")) (ByLabel "label1")
  rhs <- concatTensor (concatTensor t1 t2 (ByLabel "label1")) t3 (ByLabel "label1")
  rewrite "Concat(A, Concat(B, C)) ⇒ Concat(Concat(A, B), C)" lhs rhs

rule03 :: forall a. AnyDTypeRule a
rule03 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  adim0size <- newMap "adim0size" adim0
  [adim1size0, adim1size1, adim1size2] <- newMaps ["adim1size0", "adim1size1", "adim1size2"] adim1
  t1 <- newTensor @a "t1" [adim0 --> adim0size, adim1 --> adim1size0 @@ "label1"]
  t2 <- newTensor @a "t2" [adim0 --> adim0size, adim1 --> adim1size1 @@ "label1"]
  t3 <- newTensor @a "t3" [adim0 --> adim0size, adim1 --> adim1size2 @@ "label1"]
  lhs <- concatTensorList [t1, t2, t3] (ByLabel "label1")
  rhs <- concatTensor t1 (concatTensor t2 t3 (ByLabel "label1")) (ByLabel "label1")
  rewrite "ConcatList(A, B, C) ⇒ Concat(A, Concat(B, C))" lhs rhs

rule04 :: forall a. AnyDTypeRule a
rule04 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  [adim0sizeMap, adim0endMap, adim0startMap, adim0strideMap] <- newMaps ["adim0sizeMap", "adim0endMap", "adim0startMap", "adim0strideMap"] adim0
  [adim1sizeMap, adim1startMap1, adim1endMap1, adim1startMap2, adim1endMap2, adim1strideMap] <- newMaps ["adim1sizeMap", "adim1startMap1", "adim1endMap1", "adim1startMap2", "adim1endMap2", "adim1strideMap"] adim1
  tensor <- newTensor @a "tensor" [adim0 --> adim0sizeMap, adim1 --> adim1sizeMap]
  lhs <- concatTensor (slice tensor [adim0 --> adim0startMap, adim1 --> adim1startMap1] [adim0 --> adim0endMap, adim1 --> adim1endMap1] [adim0 --> adim0strideMap, adim1 --> adim1strideMap]) (slice tensor [adim0 --> adim0startMap, adim1 --> adim1startMap2] [adim0 --> adim0endMap, adim1 --> adim1endMap2] [adim0 --> adim0strideMap, adim1 --> adim1strideMap]) (ByAdim adim1)
  rhs <- slice tensor [adim0 --> adim0startMap, adim1 --> adim1startMap1] [adim0 --> adim0endMap, adim1 --> adim1endMap2] [adim0 --> adim0strideMap, adim1 --> adim1strideMap]
  precondition [adim1strideMap] $ \[adim1stride] -> adim1stride .== 1
  precondition [adim1startMap1, adim1startMap2] $
    \[adim1start1, adim1start2] -> adim1start1 .<= adim1start2
  precondition [adim1endMap1, adim1endMap2] $
    \[adim1end1, adim1end2] -> adim1end1 .<= adim1end2
  precondition [adim1endMap1, adim1startMap2] $
    \[adim1end1, adim1start2] -> adim1end1 .== adim1start2
  rewrite "Concat(Slice(A), Slice(A)) ⇒ Slice(A)" lhs rhs

rule05 :: forall a. AnyDTypeRule a
rule05 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  [sizeMap0, lowMap0, zeroMap0] <- newMaps ["sizeMap0", "lowMap0", "zeroMap0"] adim0
  [sizeMap1, zeroMap1] <- newMaps ["sizeMap1", "zeroMap1"] adim1
  tensor <- newTensor @a "tensor" [adim0 --> sizeMap0, adim1 --> sizeMap1]
  lhs <- concatTensor (constant @a "a" [adim0 --> lowMap0, adim1 --> sizeMap1]) tensor (ByAdim adim0)
  rhs <- pad tensor ("a" :: a) [adim0 --> lowMap0, adim1 --> zeroMap1] [adim0 --> zeroMap0, adim1 --> zeroMap1] [adim0 --> zeroMap0, adim1 --> zeroMap1]
  precondition [lowMap0] $ \[low0] -> low0 .>= 0
  precondition [zeroMap0] $ \[zero0] -> zero0 .== 0
  precondition [zeroMap1] $ \[zero1] -> zero1 .== 0
  rewrite "Concat(Broadcast(Scalar), B) ⇒ Pad(B, scalar, low)" lhs rhs

rule06 :: forall a. AnyDTypeRule a
rule06 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  [sizeMap0, highMap0, zeroMap0] <- newMaps ["sizeMap0", "highMap0", "zeroMap0"] adim0
  [sizeMap1, zeroMap1] <- newMaps ["sizeMap1", "zeroMap1"] adim1
  tensor <- newTensor @a "tensor" [adim0 --> sizeMap0, adim1 --> sizeMap1]
  lhs <- concatTensor tensor (constant @a "a" [adim0 --> highMap0, adim1 --> sizeMap1]) (ByAdim adim0)
  rhs <- pad tensor ("a" :: a) [adim0 --> zeroMap0, adim1 --> zeroMap1] [adim0 --> zeroMap0, adim1 --> zeroMap1] [adim0 --> highMap0, adim1 --> zeroMap1]
  precondition [highMap0] $ \[high0] -> high0 .>= 0
  precondition [zeroMap0] $ \[zero0] -> zero0 .== 0
  precondition [zeroMap1] $ \[zero1] -> zero1 .== 0
  rewrite "Concat(A, Broadcast(Scalar)) ⇒ Pad(A, scalar, high)" lhs rhs

rule07 :: forall a. AnyDTypeRule a
rule07 _ = do
  [adim0, adim1] <- newAdims ["adim0", "adim1"]
  adim0size <- newMap "adim0size" adim0
  [adim1size0, adim1size1, adim1size2] <- newMaps ["adim1size0", "adim1size1", "adim1size2"] adim1
  t1 <- newTensor @a "t1" [adim0 --> adim0size, adim1 --> adim1size0 @@ "label1"]
  t2 <- newTensor @a "t2" [adim0 --> adim0size, adim1 --> adim1size1 @@ "label1"]
  t3 <- newTensor @a "t3" [adim0 --> adim0size, adim1 --> adim1size2 @@ "label1"]
  lhs <- concatTensor t1 (concatTensor t2 t3 (ByLabel "label1")) (ByLabel "label1")
  rhs <- concatTensorList [t1, t2, t3] (ByLabel "label1")
  rewrite "Concat(A, Concat(B, C)) ⇒ ConcatList(A, B, C)" lhs rhs

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
  print "############################## rule06 ##############################"
  verifyAnyDTypeDSL rule06
  print "############################## rule07 ##############################"
  verifyAnyDTypeDSL rule07
