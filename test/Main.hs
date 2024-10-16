module Main (main) where

import Core.LinearizationTest (linearizationTest)
import Core.TensorTest (tensorTest)
import Test.Framework (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    [ testGroup "Core" [linearizationTest, tensorTest]
    ]
