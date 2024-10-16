{-# LANGUAGE OverloadedStrings #-}

module Core.LinearizationTest (linearizationTest) where

import Grisette (Solvable (con), mrgReturn)
import TensorRight.Internal.Core.Axis (Axis (Axis), AxisMapLike (fromKVPairs))
import TensorRight.Internal.Core.Linearization (delinearize, linearize)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck
  ( NonNegative (NonNegative),
    Positive (Positive),
    ioProperty,
  )
import TestUtil (eqWhenSuccess)

axisa :: Axis
axisa = Axis "a"

axisb :: Axis
axisb = Axis "b"

axisc :: Axis
axisc = Axis "c"

linearizationTest :: Test
linearizationTest =
  testGroup
    "Linearization"
    [ testProperty "linearization and delinearization are inverse" $
        \(Positive as)
         (Positive bs)
         (Positive cs)
         (NonNegative ai)
         (NonNegative bi)
         (NonNegative ci) -> ioProperty $ do
            let layout = [axisa, axisb, axisc]
            let sizes =
                  fromKVPairs
                    [ (axisa, con $ as + ai),
                      (axisb, con $ bs + bi),
                      (axisc, con $ cs + ci)
                    ]
            let indices =
                  fromKVPairs
                    [(axisa, con ai), (axisb, con bi), (axisc, con ci)]
            let linearized = linearize layout sizes indices
            let delinearized = delinearize layout sizes linearized
            delinearized `eqWhenSuccess` indices,
      testCase "linearize" $ do
        let sizes = fromKVPairs [(axisa, con 5), (axisb, con 6), (axisc, con 7)]
        let indices =
              fromKVPairs [(axisa, con 2), (axisb, con 3), (axisc, con 4)]
        let layout = [axisa, axisb, axisc]
        let expected = 2 * 6 * 7 + 3 * 7 + 4
        linearize layout sizes indices @?= expected,
      testCase "delinearize" $ do
        let sizes = fromKVPairs [(axisa, con 5), (axisb, con 6), (axisc, con 7)]
        let linearized = 2 * 6 * 7 + 3 * 7 + 4
        let layout = [axisa, axisb, axisc]
        let expected =
              fromKVPairs [(axisa, con 2), (axisb, con 3), (axisc, con 4)]
        delinearize layout sizes linearized @?= mrgReturn expected
    ]
