{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TensorRight.Internal.Core.Linearization
  ( linearize,
    delinearize,
  )
where

import Control.Exception (ArithException)
import qualified Data.HashMap.Lazy as HM
import Grisette
  ( SafeDiv (safeDiv, safeMod),
    SymInteger,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgModifyError)
import TensorRight.Internal.Core.Axis
  ( Axis,
    AxisMapLike
      ( fromKVPairs
      ),
    Indices,
    Sizes,
    getAxis,
  )
import TensorRight.Internal.Util.Error (ErrorEnv)

delinearize :: [Axis] -> Sizes -> SymInteger -> ErrorEnv Indices
delinearize layout dims v = do
  let sizesOrdered = map (`getAxis` dims) layout
  let sizesOrderedAdj = tail sizesOrdered
  let linearizationFactors = scanr (*) 1 sizesOrderedAdj
  let linearizationFactorsHash = HM.fromList $ zip layout linearizationFactors

  let delinearizeAxis axis =
        mrgModifyError (\(_ :: ArithException) -> "Division by zero") $ do
          x <- safeDiv v $ linearizationFactorsHash HM.! axis
          r <- safeMod x $ getAxis axis dims
          mrgReturn (axis, r)
  delinearized <- mapM delinearizeAxis layout
  mrgReturn $ fromKVPairs delinearized

linearize :: [Axis] -> Sizes -> Indices -> SymInteger
linearize layout dims indices =
  sum $
    map
      (\dim -> getAxis dim indices * linearizationFactorsHash HM.! dim)
      layout
  where
    sizesOrdered = map (`getAxis` dims) layout
    sizesOrderedAdj = tail sizesOrdered
    linearizationFactors = scanr (*) 1 sizesOrderedAdj
    linearizationFactorsHash = HM.fromList $ zip layout linearizationFactors
