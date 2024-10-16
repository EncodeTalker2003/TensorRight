module Main (main) where

import Grisette hiding ((-->))
import TensorRight

rulePadTwice :: forall a. AnyDTypeRule a
rulePadTwice _ = do
  adim <- newAdim "adim"

  shape <- newMap "shape" adim

  [innerLow, innerInterior, innerHigh] <-
    newMaps ["innerLow", "innerInterior", "innerHigh"] adim
  [outerLow, outerInterior, outerHigh] <-
    newMaps ["outerLow", "outerInterior", "outerHigh"] adim
  [rhsLow, rhsInterior, rhsHigh] <-
    newMaps ["rhsLow", "rhsInterior", "rhsHigh"] adim

  let cond outerPad innerPad rhsPad =
        precondition [outerPad, innerPad, rhsPad] $
          \[vouterPad, vinnerPad, vrhsPad] ->
            vrhsPad .== vouterPad + vinnerPad

  cond outerLow innerLow rhsLow
  cond outerInterior innerInterior rhsInterior
  cond outerHigh innerHigh rhsHigh

  precondition [outerInterior] $ \[vi0] -> vi0 .== 0
  precondition [innerLow] $ \[vi0] -> vi0 .>= 0
  precondition [innerHigh] $ \[vi0] -> vi0 .>= 0

  x <- newTensor @a "x" [adim --> shape]

  lhs <-
    pad
      ( pad x ("a" :: a) $
          Padding
            { low = [adim --> innerLow],
              interior = [adim --> innerInterior],
              high = [adim --> innerHigh]
            }
      )
      ("a" :: a)
      $ Padding
        { low = [adim --> outerLow],
          interior = [adim --> outerInterior],
          high = [adim --> outerHigh]
        }
  rhs <-
    pad x ("a" :: a) $
      Padding
        { low = [adim --> rhsLow],
          interior = [adim --> rhsInterior],
          high = [adim --> rhsHigh]
        }
  rewrite
    "when i0 == 0, pad(pad(x, l0, i0, h0), l1, i1, h1) --> pad(x, l0+l1, i0+i1, h0+h1)"
    lhs
    rhs
  
ruleFoldPad :: forall a. NumRule a
ruleFoldPad _ = do
  spatial <- newAdim "spatial"
  inputShape <- newMap "inputShape" spatial

  inputs <-
    newTensor @a
      "inputs"
      [spatial --> inputShape]
  
  [low, high] <- newNonNegMaps ["low", "high"] spatial
  ldilation <- newMap "ldilation" spatial
  [plow, phigh] <- newNonNegMaps ["plow", "phigh"] spatial
  pint <- newMap "pint" spatial
  newlow <- combineMap "newlow" (\[a, b, c] -> a + b * c) [low, plow, ldilation]
  newint <- combineMap "newint" (\[a, b] -> a + a * b - 1) [ldilation, pint]
  newhigh <- combineMap "newhigh" (\[a, b, c] -> a + b * c) [high, phigh, ldilation]
  interior <- combineMap "interior" (\[a] -> a - 1) [ldilation]

  lhsInputsPadded <-
    pad inputs (0 :: a) $
      Padding
        { low = [spatial --> plow],
          interior = [spatial --> pint],
          high = [spatial --> phigh]
        }
  lhs <-
    pad lhsInputsPadded (0 :: a) $
      Padding
        { low = [spatial --> low],
          interior = [spatial --> interior],
          high = [spatial --> high]
        }
  
  rhs <-
    pad inputs (0 :: a) $
      Padding
        { low = [spatial --> newlow],
          interior = [spatial --> newint],
          high = [spatial --> newhigh]
        }
  
  precondition [inputShape] $ \[s] -> s .> 0
  
  rewrite "" lhs rhs

main :: IO ()
main = do
  verifyAnyDTypeDSL rulePadTwice
  verifyNumDSL ruleFoldPad
