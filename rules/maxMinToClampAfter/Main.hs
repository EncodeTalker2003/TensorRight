module Main (main) where

import Grisette hiding ((-->))
import TensorRight
import TensorRight.Internal.Core.Tensor.TensorInt (tensorValLt)
import Control.Monad.Except (runExceptT)

maxMinToClamp :: forall a. NumRule a
maxMinToClamp _ = do
  adim <- newAdim "adim"
  map <- newMap "map" adim
  a <- newTensor @a "a" [adim --> map]
  const1 <- newTensor @a "const1" [adim --> map]
  const2 <- newTensor @a "const2" [adim --> map]
  forallIdx <- newMap "forallIdx" adim
  numTensorAssumption
    [const1, const2]
    forallIdx
    (\[c1, c2] -> simpleMerge $ do
      u <- runExceptT $ tensorValLt c1 c2
      case u of
        Left _ -> con True
        Right v -> return v
      )
  lhs <- numBinOp Max const1 (numBinOp Min a const2)
  rhs <- clamp const1 a const2
  rewrite "Max(Broadcast(Const), Min(A, Broadcast(Const2))) ⇒ Clamp(A,Const,Const2)" lhs rhs

main :: IO ()
main = do
  print "############################## MaxMinToClamp ##############################"
  verifyNumDSL maxMinToClamp
