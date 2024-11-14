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
  
rulePadLowCombine :: forall a. AnyDTypeRule a
rulePadLowCombine _ = do
  adim <- newAdim "adim"

  shape <- newMap "shape" adim

  [innerLow, outerLow, rhsLow] <-
    newMaps ["innerLow", "outerLow", "rhsLow"] adim

  let cond outerPad innerPad rhsPad =
        precondition [outerPad, innerPad, rhsPad] $
          \[vouterPad, vinnerPad, vrhsPad] ->
            vrhsPad .== vouterPad + vinnerPad

  cond outerLow innerLow rhsLow
  precondition [innerLow] $ \[vi0] -> vi0 .>= 0
  precondition [outerLow] $ \[vi0] -> vi0 .>= 0

  x <- newTensor @a "x" [adim --> shape]

  lhs <-
    padLow
      (padLow x ("a" :: a) [adim --> innerLow])
        ("a" :: a) [adim --> outerLow]

  rhs <-
    padLow x ("a" :: a) [adim --> rhsLow]
  rewrite
    "padLow(padLow(x, l0), l1) --> padLow(x, l0+l1)"
    lhs
    rhs

main :: IO ()
main = do
  verifyAnyDTypeDSL rulePadTwice
  verifyAnyDTypeDSL rulePadLowCombine
