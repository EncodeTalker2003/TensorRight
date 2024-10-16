module Main (main) where

import Data.Proxy
import qualified Data.Text as T
import Grisette hiding (dot, (-->))
import TensorRight

rule00 :: forall a. NumRule a
rule00 _ = do
  [batch, feature, output, spatial] <-
    newAdims ["batch", "feature", "output", "spatial"]

  batchShape <- newMap "batchShape" batch
  featureShape <- newMap "featureShape" feature
  outputShape <- newMap "outputShape" output
  inputSpatialShape <- newMap "inputSpatialShape" spatial
  weightSpatialShape <- newMap "weightSpatialShape" spatial

  inputs <-
    newTensor @a
      "inputs"
      [ batch --> batchShape,
        feature --> featureShape,
        spatial --> inputSpatialShape
      ]
  weights <-
    newTensor @a
      "weights"
      [ feature --> featureShape,
        output --> outputShape,
        spatial --> weightSpatialShape
      ]

  strides <- newConstMap "strides" 1 spatial
  -- Turns out that non-negative is required, even if we support negative
  -- padding. The reason is that when x contains negative indices,
  -- and x + y >= 0, then
  --
  -- pad(pad(A, x), y) /= pad(A, x + y)
  [low, high] <- newNonNegMaps ["low", "high"] spatial
  ldilation <- newConstMap "ldilation" 1 spatial
  rdilation <- newMap "rdilation" spatial
  [plow, phigh] <- newNonNegMaps ["plow", "phigh"] spatial
  pint <- newMap "pint" spatial
  newlow <- sumMap "newlow" [low, plow]
  newint <- combineMap "newlow" (\[a, b] -> a + b) [ldilation, pint]
  newhigh <- combineMap "newhigh" sum [high, phigh]

  [siMapLhsFeature, siMapRhsFeature] <-
    newMaps ["siMapLhsFeature", "siMapRhsFeature"] feature
  [siMapLhsSpatial, siMapRhsSpatial] <-
    newMaps ["siMapLhsSpatial", "siMapRhsSpatial"] spatial

  lhsInputsPadded <-
    pad inputs (0 :: a) $
      Padding
        { low = [spatial --> plow],
          interior = [spatial --> pint],
          high = [spatial --> phigh]
        }
  lhs <-
    conv
      lhsInputsPadded
      weights
      ConvConfig
        { batchAdims = [ByAdim batch],
          featureAdims = [ByAdim feature],
          outputFeatureAdims = [ByAdim output],
          strides = [spatial --> strides],
          contractingSIMaps =
            [feature --> siMapLhsFeature, spatial --> siMapLhsSpatial]
        }
      ConvPadding
        { low = [spatial --> low],
          ldilation = [spatial --> ldilation],
          high = [spatial --> high],
          rdilation = [spatial --> rdilation]
        }
  monitorExprOnFailure "inputs" inputs
  monitorExprOnFailure "weights" weights
  monitorExprOnFailure "lhsInputPadded" lhsInputsPadded
  monitorExprOnFailure "lhs" lhs
  monitorMapOnFailure "plow" (ByAdim spatial) plow
  monitorMapOnFailure "pint" (ByAdim spatial) pint
  monitorMapOnFailure "phigh" (ByAdim spatial) phigh
  monitorMapOnFailure "low" (ByAdim spatial) low
  monitorMapOnFailure "ldilation" (ByAdim spatial) ldilation
  monitorMapOnFailure "high" (ByAdim spatial) high

  monitorMapOnFailure "newlow" (ByAdim spatial) newlow
  monitorMapOnFailure "newint" (ByAdim spatial) newint
  monitorMapOnFailure "newhigh" (ByAdim spatial) newhigh
  monitorMapOnFailure "rdilation" (ByAdim spatial) rdilation

  rhs <-
    conv
      inputs
      weights
      ConvConfig
        { batchAdims = [ByAdim batch],
          featureAdims = [ByAdim feature],
          outputFeatureAdims = [ByAdim output],
          strides = [spatial --> strides],
          contractingSIMaps =
            [feature --> siMapRhsFeature, spatial --> siMapRhsSpatial]
        }
      ConvPadding
        { low = [spatial --> newlow],
          ldilation = [spatial --> newint],
          high = [spatial --> newhigh],
          rdilation = [spatial --> rdilation]
        }

  siRelation
    [siMapLhsFeature, siMapRhsFeature]
    $ \[vsiMapLhsFeature, vsiMapRhsFeature] ->
      vsiMapLhsFeature .== vsiMapRhsFeature
  siRelation
    [siMapLhsSpatial, siMapRhsSpatial]
    $ \[vsiMapLhsSpatial, vsiMapRhsSpatial] ->
      vsiMapLhsSpatial .== vsiMapRhsSpatial
  checkSIMap
    [siMapLhsFeature, siMapLhsSpatial]
    [siMapRhsFeature, siMapRhsSpatial]
  rewrite
    ( T.intercalate
        "\n"
        [ "Conv(Pad(input, innerLow, innerInt, innerHigh), weights, convLow, 0, convHigh, rdilation)",
          " ⇒ ",
          "Conv(input, weights, convLow + innerLow, innerInt, convHigh + innerHigh, rdilation)"
        ]
    )
    lhs
    rhs

rule01 :: forall a. NumRule a
rule01 _ = do
  [batch, feature, output, spatial] <-
    newAdims ["batch", "feature", "output", "spatial"]

  batchShape <- newMap "batchShape" batch
  featureShape <- newMap "featureShape" feature
  outputShape <- newMap "outputShape" output
  inputSpatialShape <- newMap "inputSpatialShape" spatial
  weightSpatialShape <- newMap "weightSpatialShape" spatial

  inputs <-
    newTensor @a
      "inputs"
      [ batch --> batchShape,
        feature --> featureShape,
        spatial --> inputSpatialShape
      ]
  weights <-
    newTensor @a
      "weights"
      [ feature --> featureShape,
        output --> outputShape,
        spatial --> weightSpatialShape
      ]

  strides <- newConstMap "strides" 1 spatial
  [convLow, convHigh] <- newNonNegMaps ["convLow", "convHigh"] spatial
  ldilation <- newConstMap "ldilation" 0 spatial
  [innerLow, innerHigh] <- newConstMaps ["innerLow", "innerHigh"] 0 spatial
  innerInt <- newMap "innerInt" spatial
  lhsRdilation <- newConstMap "lhsRdilation" 0 spatial

  [siMapLhsFeature, siMapRhsFeature] <-
    newMaps ["siMapLhsFeature", "siMapRhsFeature"] feature
  [siMapLhsSpatial, siMapRhsSpatial] <-
    newMaps ["siMapLhsSpatial", "siMapRhsSpatial"] spatial

  lhsWeightPadded <-
    pad weights (0 :: a) $
      Padding
        { low = [spatial --> innerLow],
          interior = [spatial --> innerInt],
          high = [spatial --> innerHigh]
        }
  lhs <-
    conv
      inputs
      lhsWeightPadded
      ConvConfig
        { batchAdims = [ByAdim batch],
          featureAdims = [ByAdim feature],
          outputFeatureAdims = [ByAdim output],
          strides = [spatial --> strides],
          contractingSIMaps =
            [feature --> siMapLhsFeature, spatial --> siMapLhsSpatial]
        }
      ConvPadding
        { low = [spatial --> convLow],
          ldilation = [spatial --> ldilation],
          high = [spatial --> convHigh],
          rdilation = [spatial --> lhsRdilation]
        }

  rhs <-
    conv
      inputs
      weights
      ConvConfig
        { batchAdims = [ByAdim batch],
          featureAdims = [ByAdim feature],
          outputFeatureAdims = [ByAdim output],
          strides = [spatial --> strides],
          contractingSIMaps =
            [feature --> siMapRhsFeature, spatial --> siMapRhsSpatial]
        }
      ConvPadding
        { low = [spatial --> convLow],
          ldilation = [spatial --> ldilation],
          high = [spatial --> convHigh],
          rdilation = [spatial --> innerInt]
        }

  siRelation
    [siMapLhsFeature, siMapRhsFeature]
    $ \[vsiMapLhsFeature, vsiMapRhsFeature] -> vsiMapLhsFeature .== vsiMapRhsFeature
  siRelation
    [siMapLhsSpatial, siMapRhsSpatial]
    $ \[vsiMapLhsSpatial, vsiMapRhsSpatial] -> vsiMapLhsSpatial .== vsiMapRhsSpatial
  checkSIMap
    [siMapLhsFeature, siMapLhsSpatial]
    [siMapRhsFeature, siMapRhsSpatial]
  rewrite
    ( T.intercalate
        "\n"
        [ "Conv(input, Pad(weights, 0, innerInt, 0), stride=1, convLow, ldilation, convHigh, 0)",
          " ⇒ ",
          "Conv(input, weights, stride=1, convLow, ldilation, convHigh, innerInt)"
        ]
    )
    lhs
    rhs

{-
-- Skipped because the set being reduced in lhs and rhs are different
rule02 :: forall a. NumRule a
rule02 _ = do
  [batch, feature, output, spatial] <-
    newAdims ["batch", "feature", "output", "spatial"]

  batchShape <- newMap "batchShape" batch
  featureShape <- newMap "featureShape" feature
  outputShape <- newMap "outputShape" output
  inputSpatialShape <- newMap "inputSpatialShape" spatial
  weightSpatialShape <- newMap "weightSpatialShape" spatial

  inputs <-
    newTensor @a
      "inputs"
      [ batch --> batchShape,
        feature --> featureShape,
        spatial --> inputSpatialShape
      ]
  weights <-
    newTensor @a
      "weights"
      [ feature --> featureShape,
        output --> outputShape,
        spatial --> weightSpatialShape
      ]
  [low, high, ldilation, rdilation] <-
    newMaps ["low", "high", "ldilation", "rdilation"] spatial
  strides <- newConstMap "strides" 1 spatial

  dilatedKernelSize <-
    combineMap
      "dilatedKernelSize"
      (\[w, r] -> 1 + r * (w - 1))
      [weightSpatialShape, rdilation]
  dilatedInputSize <-
    combineMap
      "dilatedInputSize"
      (\[w, r] -> 1 + r * (w - 1))
      [inputSpatialShape, ldilation]
  newlow <-
    combineMap
      "newlow"
      (\[low, di, dk] -> low + di - dk)
      [low, dilatedInputSize, dilatedKernelSize]
  newhigh <-
    combineMap
      "newhigh"
      (\[high, di, dk] -> high + dk - di)
      [high, dilatedInputSize, dilatedKernelSize]
  newldilation <- combineMap "newldilation" head [rdilation]
  newrdilation <- combineMap "newrdilation" head [ldilation]

  newstrides <- newConstMap "newstrides" 1 spatial

  [siMapLhsFeature, siMapRhsFeature] <-
    newMaps ["siMapLhsFeature", "siMapRhsFeature"] feature
  [siMapLhsSpatial, siMapRhsSpatial] <-
    newMaps ["siMapLhsSpatial", "siMapRhsSpatial"] spatial

  lhs <-
    conv
      inputs
      weights
      ConvConfig
        { batchAdims = [ByAdim batch],
          featureAdims = [ByAdim feature],
          outputFeatureAdims = [ByAdim output],
          strides = [spatial --> strides],
          contractingSIMaps =
            [ feature --> siMapLhsFeature,
              spatial --> siMapLhsSpatial
            ]
        }
      ConvPadding
        { low = [spatial --> low],
          ldilation = [spatial --> ldilation],
          high = [spatial --> high],
          rdilation = [spatial --> rdilation]
        }
  rhs <-
    conv
      (reverseTensor weights [ByAdim spatial])
      (reverseTensor inputs [ByAdim spatial])
      ConvConfig
        { batchAdims = [ByAdim batch],
          featureAdims = [ByAdim output],
          outputFeatureAdims = [ByAdim feature],
          strides = [spatial --> newstrides],
          contractingSIMaps =
            [ feature --> siMapRhsFeature,
              spatial --> siMapRhsSpatial
            ]
        }
      ConvPadding
        { low = [spatial --> newlow],
          ldilation = [spatial --> newldilation],
          high = [spatial --> newhigh],
          rdilation = [spatial --> newrdilation]
        }

  siRelation [siMapLhsFeature, siMapRhsFeature] $
    \[vsiMapLhsFeature, vsiMapRhsFeature] ->
      vsiMapLhsFeature .== vsiMapRhsFeature

  rewrite
    ( "Conv(input, weights, stride, low, ldilation, high, rdilation) "
        <> "⇒"
        <> " Conv(weights, input, stride, newlow, newldilation, newhigh, newrdilation)"
    )
    lhs
    rhs
    -}

rule03 :: forall a. NumRule a
rule03 _ = do
  [batch, feature, output, spatialTrivial, spatialOne] <-
    newAdims ["batch", "feature", "output", "spatialTrivial", "spatialOne"]

  batchShape <- newMap "batchShape" batch
  featureShape <- newMap "featureShape" feature
  outputShape <- newMap "outputShape" output
  spatialTrivialShape <- newMap "spatialTrivialShape" spatialTrivial
  inputSpatialOneShape <- newMap "inputSpatialOneShape" spatialOne
  weightSpatialOneShape <- newConstMap "weightSpatialOneShape" 1 spatialOne

  inputs <-
    newTensor @a
      "inputs"
      [ batch --> batchShape,
        feature --> featureShape,
        spatialTrivial --> spatialTrivialShape,
        spatialOne --> inputSpatialOneShape
      ]
  weights <-
    newTensor @a
      "weights"
      [ feature --> featureShape,
        output --> outputShape,
        spatialTrivial --> spatialTrivialShape,
        spatialOne --> weightSpatialOneShape
      ]
  [lowTrivial, highTrivial] <- newConstMaps ["low", "high"] 0 spatialTrivial
  [ldilationTrivial, rdilationTrivial] <-
    newConstMaps ["ldilation", "rdilation"] 1 spatialTrivial
  stridesTrivial <- newConstMap "strides" 1 spatialTrivial

  [lowOne, highOne] <- newConstMaps ["low", "high"] 0 spatialOne
  [ldilationOne, rdilationOne] <-
    newConstMaps ["ldilation", "rdilation"] 1 spatialOne
  stridesOne <- newConstMap "strides" 1 spatialOne

  [siMapLhsFeature, siMapRhsFeature] <-
    newMaps ["siMapLhsFeature", "siMapRhsFeature"] feature
  [siMapLhsSpatialTrivial, siMapRhsSpatialTrivial] <-
    newMaps
      ["siMapLhsSpatialTrivial", "siMapRhsSpatialTrivial"]
      spatialTrivial
  [siMapLhsSpatialOne] <-
    newMaps ["siMapLhsSpatialOne"] spatialOne

  lhs <-
    conv
      inputs
      weights
      ConvConfig
        { batchAdims = [ByAdim batch],
          featureAdims = [ByAdim feature],
          outputFeatureAdims = [ByAdim output],
          strides =
            [ spatialTrivial --> stridesTrivial,
              spatialOne --> stridesOne
            ],
          contractingSIMaps =
            [ feature --> siMapLhsFeature,
              spatialTrivial --> siMapLhsSpatialTrivial,
              spatialOne --> siMapLhsSpatialOne
            ]
        }
      ConvPadding
        { low = [spatialTrivial --> lowTrivial, spatialOne --> lowOne],
          ldilation = [spatialTrivial --> ldilationTrivial, spatialOne --> ldilationOne],
          high = [spatialTrivial --> highTrivial, spatialOne --> highOne],
          rdilation = [spatialTrivial --> rdilationTrivial, spatialOne --> rdilationOne]
        }

  weightExcludeSpatialOne <- reshapeDegenerate weights [] [ByAdim spatialOne]
  weightBroadcastSpatialOne <-
    broadcast weightExcludeSpatialOne [spatialOne --> inputSpatialOneShape]
  rhs <-
    reshapeDegenerate
      ( dot
          inputs
          weightBroadcastSpatialOne
          [ spatialTrivial --> siMapRhsSpatialTrivial,
            feature --> siMapRhsFeature
          ]
          [ByAdim spatialOne]
      )
      [(ByAdim spatialTrivial, spatialTrivial)]
      []
  siRelation [siMapLhsFeature, siMapRhsFeature] $
    \[vsiMapLhsFeature, vsiMapRhsFeature] ->
      vsiMapLhsFeature .== vsiMapRhsFeature
  siRelation [siMapLhsSpatialTrivial, siMapRhsSpatialTrivial] $
    \[vsiMapLhsSpatialTrivial, vsiMapRhsSpatialTrivial] ->
      vsiMapLhsSpatialTrivial .== vsiMapRhsSpatialTrivial
  siRelation [siMapLhsSpatialOne] $ \[vsiMapLhsSpatialOne] ->
    vsiMapLhsSpatialOne .== 0
  checkSIMap
    [siMapLhsFeature, siMapLhsSpatialTrivial, siMapLhsSpatialOne]
    [siMapRhsFeature, siMapRhsSpatialTrivial]
  monitorExprOnFailure "rhs" rhs
  monitorMapOnFailure "lso" (ByAdim spatialOne) siMapLhsSpatialOne
  monitorMapOnFailure "lst" (ByAdim spatialTrivial) siMapLhsSpatialTrivial
  monitorMapOnFailure "rst" (ByAdim spatialTrivial) siMapRhsSpatialTrivial

  rewrite "Conv(A,B) => Dot(A, B)" lhs rhs

main :: IO ()
main = do
  verifyNumDSL rule00
  verifyNumDSL rule01
  verifyNumDSL rule03
