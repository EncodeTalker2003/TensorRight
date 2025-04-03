module Main (main) where

import Grisette hiding ((-->))
import TensorRight

rulePadTwice :: forall a. AnyDTypeRule a
rulePadTwice _ = do
  rclass <- newRClass "rclass"

  shape <- newMap "shape" rclass

  [innerLow, innerInterior, innerHigh] <-
    newMaps ["innerLow", "innerInterior", "innerHigh"] rclass
  [outerLow, outerInterior, outerHigh] <-
    newMaps ["outerLow", "outerInterior", "outerHigh"] rclass
  [rhsLow, rhsInterior, rhsHigh] <-
    newMaps ["rhsLow", "rhsInterior", "rhsHigh"] rclass

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

  x <- newTensor @a "x" [rclass --> shape]

  lhs <-
    pad
      ( pad x ("a" :: a) $
          Padding
            { low = [rclass --> innerLow],
              interior = [rclass --> innerInterior],
              high = [rclass --> innerHigh]
            }
      )
      ("a" :: a)
      $ Padding
        { low = [rclass --> outerLow],
          interior = [rclass --> outerInterior],
          high = [rclass --> outerHigh]
        }
  rhs <-
    pad x ("a" :: a) $
      Padding
        { low = [rclass --> rhsLow],
          interior = [rclass --> rhsInterior],
          high = [rclass --> rhsHigh]
        }
  rewrite
    "when i0 == 0, pad(pad(x, l0, i0, h0), l1, i1, h1) --> pad(x, l0+l1, i0+i1, h0+h1)"
    lhs
    rhs

rulePadLowCombine :: forall a. AnyDTypeRule a
rulePadLowCombine _ = do
  rclass <- newRClass "rclass"

  shape <- newMap "shape" rclass

  [innerLow, outerLow, rhsLow] <-
    newMaps ["innerLow", "outerLow", "rhsLow"] rclass

  let cond outerPad innerPad rhsPad =
        precondition [outerPad, innerPad, rhsPad] $
          \[vouterPad, vinnerPad, vrhsPad] ->
            vrhsPad .== vouterPad + vinnerPad

  cond outerLow innerLow rhsLow
  precondition [innerLow] $ \[vi0] -> vi0 .>= 0
  precondition [outerLow] $ \[vi0] -> vi0 .>= 0

  x <- newTensor @a "x" [rclass --> shape]

  lhs <-
    padLow
      (padLow x ("a" :: a) [rclass --> innerLow])
      ("a" :: a)
      [rclass --> outerLow]

  rhs <-
    padLow x ("a" :: a) [rclass --> rhsLow]
  rewrite
    "padLow(padLow(x, l0), l1) --> padLow(x, l0+l1)"
    lhs
    rhs

main :: IO ()
main = do
  verifyAnyDTypeDSL rulePadTwice
  verifyAnyDTypeDSL rulePadLowCombine
