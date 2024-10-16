module TestUtil (eqWhenSuccess, isError, isNotError) where

import Control.Monad.Except (runExceptT)
import Data.List (intercalate)
import GHC.Stack (HasCallStack)
import Grisette
  ( EvalSym (evalSym),
    Mergeable,
    Solvable (con),
    SolvingFailure (Unsat),
    SymEq ((./=)),
    mrgFmap,
    mrgReturn,
    simpleMerge,
    solve,
    z3,
  )
import TensorRight.Internal.Util.Error (ErrorEnv)
import Test.HUnit (assertBool, (@?=))

isError :: (HasCallStack) => (Mergeable a) => ErrorEnv a -> IO ()
isError err = do
  let actual =
        mrgFmap (either (const $ Left ()) (const $ Right ())) $ runExceptT err
  actual @?= mrgReturn (Left ())

isNotError :: (HasCallStack) => (Mergeable a, Show a) => ErrorEnv a -> IO ()
isNotError v = do
  let actual =
        mrgFmap (either (const $ Left ()) (const $ Right ())) $ runExceptT v
  assertBool ("Must not be error, but got: " <> show v) $
    actual /= mrgReturn (Left ())

eqWhenSuccess ::
  (HasCallStack, EvalSym v, Show v, SymEq v, Mergeable v) =>
  ErrorEnv v ->
  v ->
  IO ()
eqWhenSuccess actual expected = do
  isNotError actual
  let r = simpleMerge $ do
        v <- runExceptT actual
        case v of
          Left _ -> mrgReturn $ con False
          Right x -> mrgReturn $ x ./= expected
  m <- solve z3 r
  case m of
    Left Unsat -> pure ()
    Left err -> fail $ "Solver failed: " <> show err
    Right m -> do
      fail $
        intercalate
          "\n"
          [ "unexpected model: " <> show m,
            "actual: " <> show (evalSym False m actual),
            "expected: " <> show (evalSym False m expected),
            "Failed"
          ]
