module Main (main) where

import TensorRight

rule01 :: forall a. AnyDTypeRule a
rule01 _ = do
  [adim] <- newAdims ["adim"]
  map <- newMap "map" adim
  tensor <- newTensor @a "tensor" [adim --> map]
  lhs <-
    relabel
      (relabel tensor [adim --> ByLabel "label"])
      [ByLabel "label" --> ByLabel "label2"]
  rhs <- relabel tensor [adim --> ByLabel "label2"]
  rewrite "Transpose(Transpose(A)) ⇒ Transpose(A)" lhs rhs

rule02 :: forall a. AnyDTypeRule a
rule02 _ = do
  [adim] <- newAdims ["adim"]
  map <- newMap "map" adim
  tensor <- newTensor @a "tensor" [adim --> map @@ "label"]
  lhs <- relabel tensor [ByLabel "label" --> ByLabel "label"]
  let rhs = tensor
  rewrite "Transpose(A) ⇒ A" lhs rhs

main :: IO ()
main = do
  print "############################## rule01 ##############################"
  verifyAnyDTypeDSL rule01
  print "############################## rule02 ##############################"
  verifyAnyDTypeDSL rule02
