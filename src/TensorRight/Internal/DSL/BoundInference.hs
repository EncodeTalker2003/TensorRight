{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module TensorRight.Internal.DSL.BoundInference (inferBound) where

import Control.Monad (when)
import Control.Monad.State (State, execState, modify)
import Data.Function (on)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List (groupBy, sortOn)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Typeable (cast)
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    GrisetteSMTConfig,
    Identifier (IdentifierWithInfo),
    LogicalOp ((.&&)),
    PPrint (pformat),
    Solvable (con, ssym),
    SolvingFailure (Unsat),
    SymBool (SymBool),
    SymEq ((./=)),
    Symbol (SimpleSymbol),
    TypedSymbol (TypedSymbol, unTypedSymbol),
    solve,
    withInfo,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( LinkedRep (underlyingTerm),
    SupportedPrim,
    Term (AbsNumTerm, AddNumTerm, AndTerm, ApplyTerm, ConTerm, DivIntegralTerm, EqTerm, ITETerm, LeOrdTerm, LtOrdTerm, ModIntegralTerm, MulNumTerm, NegNumTerm, NotTerm, OrTerm, QuotIntegralTerm, RemIntegralTerm, SignumNumTerm, SymTerm, FdivTerm, FloatingUnaryTerm),
    pevalNEqTerm,
    pformat,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Utils (pattern Dyn)
import Grisette.Internal.SymPrim.Prim.SomeTerm (SomeTerm (SomeTerm))
import Grisette.Internal.SymPrim.Prim.TermUtils (someTermSize)
import TensorRight.Internal.Core.Axis (Axis (Axis, LabelledAxis), AxisMapLike (fromHashMap), Indices)
import TensorRight.Internal.Core.Tensor (tensorDType)
import TensorRight.Internal.Core.Verify (VerifyTask (VerifyTask), getTensorWithValidityCondition, rewritingRuleAccess)
import TensorRight.Internal.DSL.Eval (SymIdentInfo (SymMap, SymTensor), getAxisName)
import TensorRight.Internal.DSL.Identifier (RClassIdentifier, TensorIdentifier)
import TensorRight.Internal.DSL.Shape (AbstractShape (AbstractShape, labelled, unlabelled))

instance PPrint SomeTerm where
  pformat (SomeTerm t) =
    Grisette.pformat $
      Grisette.Internal.SymPrim.Prim.Internal.Term.pformat t

newtype AnalysisState = AnalysisState
  { termRClassTensors ::
      HM.HashMap
        SomeTerm
        (HS.HashSet RClassIdentifier, HS.HashSet TensorIdentifier)
  }
  deriving (Show, Generic)
  deriving (PPrint) via (Default AnalysisState)

hasRClass :: AnalysisState -> RClassIdentifier -> SomeTerm -> Bool
hasRClass AnalysisState {..} rclass st =
  case HM.lookup st termRClassTensors of
    Just (rclasses, _) -> HS.member rclass rclasses
    Nothing -> error "Term not found"

analysisTerm :: SomeTerm -> AnalysisState
analysisTerm someTerm =
  execState (analysisTermState someTerm) (AnalysisState HM.empty)

analysisTermState ::
  SomeTerm ->
  State AnalysisState (HS.HashSet RClassIdentifier, HS.HashSet TensorIdentifier)
analysisTermState someTerm = do
  (rclasses, tensors) <- analysisTermState' someTerm
  modify
    ( \s ->
        s
          { termRClassTensors =
              HM.insert someTerm (rclasses, tensors) (termRClassTensors s)
          }
    )
  return (rclasses, tensors)

analysisTermState' ::
  SomeTerm ->
  State AnalysisState (HS.HashSet RClassIdentifier, HS.HashSet TensorIdentifier)
analysisTermState' (SomeTerm t) = do
  case t of
    ConTerm {} -> return (HS.empty, HS.empty)
    SymTerm _ symb -> case unTypedSymbol symb of
      SimpleSymbol (IdentifierWithInfo _ info) ->
        case cast info of
          Just (SymMap rclass _ _) -> do
            return (HS.singleton rclass, HS.empty)
          Just (SymTensor t) -> do
            return (HS.empty, HS.singleton t)
          _ -> error "Unexpected info"
      _ -> return (HS.empty, HS.empty)
    EqTerm _ l r -> do
      (lRClasses, lTensors) <- analysisTermState $ SomeTerm l
      (rRClasses, rTensors) <- analysisTermState $ SomeTerm r
      return (lRClasses <> rRClasses, lTensors <> rTensors)
    ITETerm _ cond t e -> do
      (condRClasses, condTensors) <- analysisTermState $ SomeTerm cond
      (tRClasses, tTensors) <- analysisTermState $ SomeTerm t
      (eRClasses, eTensors) <- analysisTermState $ SomeTerm e
      return
        ( condRClasses <> tRClasses <> eRClasses,
          condTensors <> tTensors <> eTensors
        )
    NotTerm _ t -> analysisTermState $ SomeTerm t
    OrTerm _ l r -> do
      (lRClasses, lTensors) <- analysisTermState $ SomeTerm l
      (rRClasses, rTensors) <- analysisTermState $ SomeTerm r
      return (lRClasses <> rRClasses, lTensors <> rTensors)
    AndTerm _ l r -> do
      (lRClasses, lTensors) <- analysisTermState $ SomeTerm l
      (rRClasses, rTensors) <- analysisTermState $ SomeTerm r
      return (lRClasses <> rRClasses, lTensors <> rTensors)
    AddNumTerm _ l r -> do
      (lRClasses, lTensors) <- analysisTermState $ SomeTerm l
      (rRClasses, rTensors) <- analysisTermState $ SomeTerm r
      return (lRClasses <> rRClasses, lTensors <> rTensors)
    NegNumTerm _ t -> analysisTermState $ SomeTerm t
    MulNumTerm _ l r -> do
      (lRClasses, lTensors) <- analysisTermState $ SomeTerm l
      (rRClasses, rTensors) <- analysisTermState $ SomeTerm r
      return (lRClasses <> rRClasses, lTensors <> rTensors)
    AbsNumTerm _ t -> analysisTermState $ SomeTerm t
    SignumNumTerm _ t -> analysisTermState $ SomeTerm t
    LtOrdTerm _ l r -> do
      (lRClasses, lTensors) <- analysisTermState $ SomeTerm l
      (rRClasses, rTensors) <- analysisTermState $ SomeTerm r
      return (lRClasses <> rRClasses, lTensors <> rTensors)
    LeOrdTerm _ l r -> do
      (lRClasses, lTensors) <- analysisTermState $ SomeTerm l
      (rRClasses, rTensors) <- analysisTermState $ SomeTerm r
      return (lRClasses <> rRClasses, lTensors <> rTensors)
    ApplyTerm _ f args -> do
      (fRClasses, lTensors) <- analysisTermState $ SomeTerm f
      (argsRClasses, rTensors) <- analysisTermState $ SomeTerm args
      return (fRClasses <> argsRClasses, lTensors <> rTensors)
    DivIntegralTerm _ l r -> do
      (lRClasses, lTensors) <- analysisTermState $ SomeTerm l
      (rRClasses, rTensors) <- analysisTermState $ SomeTerm r
      return (lRClasses <> rRClasses, lTensors <> rTensors)
    ModIntegralTerm _ l r -> do
      (lRClasses, lTensors) <- analysisTermState $ SomeTerm l
      (rRClasses, rTensors) <- analysisTermState $ SomeTerm r
      return (lRClasses <> rRClasses, lTensors <> rTensors)
    QuotIntegralTerm _ l r -> do
      (lRClasses, lTensors) <- analysisTermState $ SomeTerm l
      (rRClasses, rTensors) <- analysisTermState $ SomeTerm r
      return (lRClasses <> rRClasses, lTensors <> rTensors)
    RemIntegralTerm _ l r -> do
      (lRClasses, lTensors) <- analysisTermState $ SomeTerm l
      (rRClasses, rTensors) <- analysisTermState $ SomeTerm r
      return (lRClasses <> rRClasses, lTensors <> rTensors)
    FdivTerm _ l r -> do
      (lRClasses, lTensors) <- analysisTermState $ SomeTerm l
      (rRClasses, rTensors) <- analysisTermState $ SomeTerm r
      return (lRClasses <> rRClasses, lTensors <> rTensors)
    FloatingUnaryTerm _ _ l -> analysisTermState $ SomeTerm l
    _ -> error "Should not happen"

getAllConditions :: AnalysisState -> SomeTerm -> HS.HashSet SomeTerm
getAllConditions AnalysisState {..} st@(SomeTerm t) =
  case HM.lookup st termRClassTensors of
    Just (rclasses, _) | HS.null rclasses -> HS.empty
    Just (_, tensors) | HS.null tensors ->
      case cast t of
        Just (_ :: Term Bool) -> HS.singleton st
        Nothing -> goBody t
    Just _ -> goBody t
    Nothing -> error "Term not found"
  where
    goSub :: forall a. (SupportedPrim a) => Term a -> HS.HashSet SomeTerm
    goSub = getAllConditions AnalysisState {..} . SomeTerm
    goBody :: forall a. (SupportedPrim a) => Term a -> HS.HashSet SomeTerm
    goBody (ConTerm _ _) = error "Should not happen"
    goBody (SymTerm _ _) = error "Should not happen"
    goBody (EqTerm _ l r) = goSub l <> goSub r
    goBody (ITETerm _ cond t e) = goSub cond <> goSub t <> goSub e
    goBody (NotTerm _ t) = goSub t
    goBody (OrTerm _ l r) = goSub l <> goSub r
    goBody (AndTerm _ l r) = goSub l <> goSub r
    goBody (AddNumTerm _ l r) = goSub l <> goSub r
    goBody (NegNumTerm _ t) = goSub t
    goBody (MulNumTerm _ l r) = goSub l <> goSub r
    goBody (AbsNumTerm _ t) = goSub t
    goBody (SignumNumTerm _ t) = goSub t
    goBody (LtOrdTerm _ l r) = goSub l <> goSub r
    goBody (LeOrdTerm _ l r) = goSub l <> goSub r
    goBody ApplyTerm {} = HS.empty
    goBody (DivIntegralTerm _ l r) = goSub l <> goSub r
    goBody (ModIntegralTerm _ l r) = goSub l <> goSub r
    goBody (QuotIntegralTerm _ l r) = goSub l <> goSub r
    goBody (RemIntegralTerm _ l r) = goSub l <> goSub r
    goBody (FdivTerm _ l r) = goSub l <> goSub r
    goBody (FloatingUnaryTerm _ _ l) = goSub l
    goBody _ = error "Should not happen"

getAllAccesses :: AnalysisState -> SomeTerm -> HS.HashSet SomeTerm
getAllAccesses AnalysisState {..} st@(SomeTerm t) =
  case t of
    ConTerm _ _ -> HS.empty
    SymTerm _ _ -> HS.empty
    EqTerm _ l r -> goSub l <> goSub r
    ITETerm _ cond t e -> goSub cond <> goSub t <> goSub e
    NotTerm _ t -> goSub t
    OrTerm _ l r -> goSub l <> goSub r
    AndTerm _ l r -> goSub l <> goSub r
    AddNumTerm _ l r -> goSub l <> goSub r
    NegNumTerm _ t -> goSub t
    MulNumTerm _ l r -> goSub l <> goSub r
    AbsNumTerm _ t -> goSub t
    SignumNumTerm _ t -> goSub t
    LtOrdTerm _ l r -> goSub l <> goSub r
    LeOrdTerm _ l r -> goSub l <> goSub r
    ApplyTerm {} -> HS.singleton st
    DivIntegralTerm _ l r -> goSub l <> goSub r
    ModIntegralTerm _ l r -> goSub l <> goSub r
    QuotIntegralTerm _ l r -> goSub l <> goSub r
    RemIntegralTerm _ l r -> goSub l <> goSub r
    FdivTerm _ l r -> goSub l <> goSub r
    FloatingUnaryTerm _ _ l -> goSub l
    _ -> error "Should not happen"
  where
    goSub :: forall a. (SupportedPrim a) => Term a -> HS.HashSet SomeTerm
    goSub = getAllAccesses AnalysisState {..} . SomeTerm

conditionEquivalent ::
  GrisetteSMTConfig -> SymBool -> SomeTerm -> SomeTerm -> IO Bool
conditionEquivalent solverConfig allPreCond (SomeTerm l) (SomeTerm r) =
  case (cast l, cast r) of
    (Just (a11 :: Term Bool), Just (a12 :: Term Bool)) -> do
      r <-
        solve solverConfig (allPreCond .&& SymBool a11 ./= SymBool a12)
      case r of
        Left Unsat -> return True
        Left err -> fail $ "Unexpected solver failure: " <> show err
        Right _ -> return False
    _ -> error "Not conditions"

accessEquivalent ::
  GrisetteSMTConfig -> SymBool -> SomeTerm -> SomeTerm -> IO Bool
accessEquivalent solverConfig allPreCond (SomeTerm l) (SomeTerm r) =
  case (l, r) of
    (ApplyTerm _ (f1 :: Term f1) (args1 :: Term args1), ApplyTerm _ f2 args2) ->
      case (cast f2, cast args2) of
        (Just (f2' :: Term f1), _) | f1 /= f2' -> return False
        (Just (_ :: Term f1), Just (args2' :: Term args1)) -> do
          r <-
            solve solverConfig (allPreCond .&& SymBool (pevalNEqTerm args1 args2'))
          case r of
            Left Unsat -> return True
            Left err -> fail $ "Unexpected solver failure: " <> show err
            Right _ -> return False
        _ -> return False
    _ -> error "Not accesses"

filterPairs ::
  (SomeTerm -> SomeTerm -> IO Bool) ->
  HS.HashSet SomeTerm ->
  IO (HS.HashSet SomeTerm)
filterPairs eqv conditions = do
  HS.fromList <$> go (sortOn (\x -> -someTermSize x) $ HS.toList conditions)
  where
    go [] = return []
    go (x : xs) = do
      r <- go1 x xs
      t <- go xs
      if r
        then return t
        else return (x : t)
    go1 _ [] = return False
    go1 x (y : ys) = do
      r <- eqv x y
      rs <- go1 x ys
      return $ r || rs

groupAccessByTensors :: [SomeTerm] -> [[SomeTerm]]
groupAccessByTensors = groupBy (on (==) termTensor) . sortOn termTensor
  where
    termTensor :: SomeTerm -> TensorIdentifier
    termTensor
      ( SomeTerm
          ( ApplyTerm
              _
              ( SymTerm
                  _
                  ( TypedSymbol
                      (SimpleSymbol (IdentifierWithInfo _ (Dyn (SymTensor t))))
                    )
                )
              _
            )
        ) = t
    termTensor _ = error "Should not happen"

inferBound ::
  GrisetteSMTConfig ->
  VerifyTask ->
  HS.HashSet RClassIdentifier ->
  HS.HashSet RClassIdentifier ->
  AbstractShape ->
  IO (HM.HashMap RClassIdentifier Int)
inferBound
  solverConfig
  (VerifyTask _ lhs rhs pre siRelation _ _ _ _ _ _ _ _ _)
  nonSingletonRClasses
  singletonRClasses
  sp = do
    let preCond = pre
    when (preCond == con False) $
      fail "verified (precondition is false)"

    (lhsTensorIsValid, lhsTensor) <-
      getTensorWithValidityCondition "lhs-tensor" lhs
    (rhsTensorIsValid, rhsTensor) <-
      getTensorWithValidityCondition "rhs-tensor" rhs

    when (tensorDType lhsTensor /= tensorDType rhsTensor) $
      fail "not verified (lhs and rhs have different types)"
    let access = abstractShapeAccess sp

    (lhsAccessIsValid, rhsAccessIsValid, equivalent) <-
      rewritingRuleAccess lhsTensor rhsTensor access
    let st = analysisTerm $ SomeTerm $ underlyingTerm equivalent
    let allPreCond =
          preCond
            .&& siRelation
            .&& lhsTensorIsValid
            .&& rhsTensorIsValid
            .&& lhsAccessIsValid
            .&& rhsAccessIsValid
    let allConditions =
          getAllConditions st $
            SomeTerm $
              underlyingTerm equivalent
    filteredConditions <-
      filterPairs (conditionEquivalent solverConfig allPreCond) allConditions
    let allAccesses = getAllAccesses st $ SomeTerm $ underlyingTerm equivalent
    filteredAccesses <- filterPairs (accessEquivalent solverConfig allPreCond) allAccesses
    putStrLn $ "# all conditions: " <> show (HS.size allConditions)
    putStrLn $ "# all accesses: " <> show (HS.size allAccesses)
    putStrLn $ "# filtered conditions: " <> show (HS.size filteredConditions)
    putStrLn $ "# filtered accesses: " <> show (HS.size filteredAccesses)
    let groupedAccesses = groupAccessByTensors $ HS.toList filteredAccesses

    let numHasRClassInGroup :: RClassIdentifier -> [SomeTerm] -> Int
        numHasRClassInGroup rclass = length . filter (hasRClass st rclass)
        factorial :: Int -> Int
        factorial 0 = 1
        factorial n = n * factorial (n - 1)
        kFromAccess :: RClassIdentifier -> [SomeTerm] -> Int
        kFromAccess rclass group = case numHasRClassInGroup rclass group of
          v | v < 2 -> 0
          v -> factorial v `div` 2 `div` factorial (v - 2)

    let kFromAllAccesses rclass = sum $ kFromAccess rclass <$> groupedAccesses
    let kForRClass rclass =
          max 1 $
            kFromAllAccesses rclass
              + numHasRClassInGroup rclass (HS.toList filteredConditions)
    return $
      HM.fromList $
        ( (\rclass -> (rclass, kForRClass rclass))
            <$> HS.toList (nonSingletonRClasses `HS.difference` singletonRClasses)
        )
          <> ((,1) <$> HS.toList singletonRClasses)

abstractShapeAccess :: AbstractShape -> Indices
abstractShapeAccess AbstractShape {..} = do
  fromHashMap $ unlabelledRClassAccesses <> labelledRClassAccess
  where
    unlabelledRClassAccesses =
      HM.fromList $
        ( \rclass ->
            ( Axis $ getAxisName rclass 0,
              ssym $ withInfo "access" $ SymMap rclass 0 "#accnolabel"
            )
        )
          <$> HS.toList unlabelled
    labelledRClassAccess =
      HM.fromList $
        ( \(label, rclass) ->
            ( LabelledAxis label $ getAxisName rclass 0,
              ssym $
                withInfo "access" $
                  SymMap rclass 0 $
                    fromString $
                      T.unpack label
            )
        )
          <$> HM.toList labelled
