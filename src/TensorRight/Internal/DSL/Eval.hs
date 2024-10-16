{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TensorRight.Internal.DSL.Eval
  ( getAxisName,
    getAxisMapLike,
    SymIdentInfo (..),
    evalRewrite,
    eval,
    EvalState (..),
    freshMapBase,
    modifyMaps,
  )
where

import Control.Arrow (Arrow (first))
import Control.DeepSeq (NFData)
import Control.Monad.State (MonadState (get), State, gets, modify')
import Data.Foldable (Foldable (toList))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.Set.Ordered as OS
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( LogicalOp (false, (.||)),
    SymBool,
    SymEq ((./=)),
    SymInteger (SymInteger),
    TypedSymbol (TypedSymbol),
    modifyIdentifier,
    withInfo,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term (Term (SymTerm), symTerm)
import Language.Haskell.TH.Syntax (Lift)
import TensorRight.Internal.Core.Axis
  ( Axes,
    Axis (Axis, LabelledAxis),
    AxisMapLike (fromHashMap),
    Indices,
    Sizes,
  )
import TensorRight.Internal.Core.Tensor
  ( DType,
    Elem (BoolElem),
    Tensor,
    boolBinOp,
    boolScalarBinOp,
    boolUnaryOp,
    broadcast,
    clamp,
    clampScalar,
    compareOp,
    concatTensor,
    concatTensorList,
    constantTensor,
    conv,
    convBase,
    createTensor,
    dot,
    dynamicSlice,
    dynamicUpdateSlice,
    iota,
    numBinOp,
    numScalarBinOp,
    numUnaryOp,
    pad,
    reduce,
    relabel,
    reshapeDegenerate,
    reverseTensor,
    select,
    slice,
  )
import TensorRight.Internal.Core.Tensor.Typed
  ( ConvConfigArgs
      ( ConvConfigArgs,
        convBatchAxes,
        convContractingSIMap,
        convFeatureAxes,
        convOutputFeatureAxes,
        convStrides
      ),
    ConvPaddingArgs
      ( ConvPaddingArgs,
        convHighPadding,
        convLDilation,
        convLowPadding,
        convRDilation
      ),
    DySliceArgs (DySliceArgs, sizes, start),
    PaddingArgs (PaddingArgs, padHigh, padInterior, padLow),
    SliceArgs (SliceArgs, end, start, strides),
    TensorElem (TensorElemVal),
  )
import TensorRight.Internal.DSL.Expr
  ( ConvConfigArgsExpr
      ( ConvConfigArgsExpr,
        batchAdims,
        contractingSIMaps,
        featureAdims,
        outputFeatureAdims,
        strides
      ),
    ConvPaddingArgsExpr (ConvPaddingArgsExpr, high, ldilation, low, rdilation),
    DySliceArgsExpr (DySliceArgsExpr, sizes, start),
    Expr
      ( BoolBinOp,
        BoolScalarBinOp,
        BoolUnaryOp,
        Broadcast,
        Clamp,
        ClampScalar,
        CompareOp,
        Concat,
        ConcatList,
        Constant,
        Conv,
        ConvBase,
        Dot,
        DynamicSlice,
        DynamicUpdateSlice,
        Iota,
        NumBinOp,
        NumScalarBinOp,
        NumUnaryOp,
        Pad,
        Reduce,
        Relabel,
        ReshapeDegenerate,
        ReverseTensor,
        Select,
        Slice,
        Var
      ),
    PaddingArgsExpr (PaddingArgsExpr, high, interior, low),
    Params,
    Rewrite (Rewrite),
    SliceArgsExpr (SliceArgsExpr, end, start, strides),
    exprId,
  )
import TensorRight.Internal.DSL.Identifier (AdimIdentifier, MapIdentifier, TensorIdentifier)
import TensorRight.Internal.DSL.Shape
  ( AbstractShape (AbstractShape),
    AdimRef (ByAdim, ByLabel),
    TensorShape (labelled, unlabelled),
    toAbstractShape,
  )
import TensorRight.Internal.Util.Error (ErrorEnv)

type MapBase = HM.HashMap T.Text SymInteger

freshMapBase :: String -> MapBase -> (SymBool, MapBase)
freshMapBase name mapBase =
  ( HM.foldlWithKey
      (\acc k v -> acc .|| (new HM.! k ./= v))
      false
      mapBase,
    new
  )
  where
    new =
      HM.map
        ( \(SymInteger (SymTerm _ (TypedSymbol s))) ->
            SymInteger $ symTerm $ modifyIdentifier (`withInfo` name) s
        )
        mapBase

modifyMaps ::
  String ->
  HS.HashSet MapIdentifier ->
  HM.HashMap MapIdentifier MapBase ->
  (SymBool, HM.HashMap MapIdentifier MapBase)
modifyMaps name maps allMaps =
  ( HM.foldlWithKey
      ( \acc mapIdent mapBase ->
          if HS.member mapIdent maps
            then acc .|| fst (freshMapBase name mapBase)
            else acc
      )
      false
      allMaps,
    new
  )
  where
    new = flip HM.mapWithKey allMaps $ \mapIdent mapBase ->
      if HS.member mapIdent maps
        then snd $ freshMapBase name mapBase
        else mapBase

data EvalState = EvalState
  { adims :: HM.HashMap AdimIdentifier (OS.OSet Int),
    maps :: HM.HashMap MapIdentifier MapBase,
    allTensorShapes :: HM.HashMap TensorIdentifier TensorShape,
    allTensorDTypes :: HM.HashMap TensorIdentifier DType,
    evaluated :: HM.HashMap Int (ErrorEnv Tensor),
    exprShapes :: HM.HashMap Int AbstractShape
  }
  deriving (Show, Generic)

type EvalContext = State EvalState

getAxisMapLike :: (AxisMapLike m) => AdimRef -> MapIdentifier -> EvalContext m
getAxisMapLike ref map = do
  maps <- gets maps
  case HM.lookup map maps of
    Just m -> case ref of
      ByAdim _ -> return $ fromHashMap $ HM.mapKeys Axis m
      ByLabel label -> return $ fromHashMap $ HM.mapKeys (LabelledAxis label) m
    Nothing -> error $ "Map " <> show map <> "not found"

getAdimByAdimRef :: AbstractShape -> AdimRef -> EvalContext AdimIdentifier
getAdimByAdimRef (AbstractShape _ u) (ByAdim adim) = do
  if HS.member adim u
    then return adim
    else error $ "Adim " <> show adim <> " not found in shape"
getAdimByAdimRef (AbstractShape l _) (ByLabel label) = do
  case HM.lookup label l of
    Nothing -> error $ "Label " <> show label <> " not found in shape"
    Just adim -> return adim

getAxisName :: AdimIdentifier -> Int -> T.Text
getAxisName adim i = T.pack $ show adim <> "." <> show i

getAxesList :: AdimRef -> AdimIdentifier -> EvalContext [Axis]
getAxesList (ByAdim adim) _ = do
  adims <- gets adims
  case HM.lookup adim adims of
    Nothing -> error $ "Adim " <> show adim <> " not found"
    Just axes ->
      return $ Axis . getAxisName adim <$> toList axes
getAxesList (ByLabel label) adim = do
  adims <- gets adims
  case HM.lookup adim adims of
    Nothing -> error $ "Adim " <> show adim <> " not found"
    Just axes ->
      return $ LabelledAxis label . getAxisName adim <$> toList axes

getAxes :: AdimRef -> AdimIdentifier -> EvalContext Axes
getAxes ref adim = HS.fromList <$> getAxesList ref adim

adimToAxes :: AbstractShape -> AdimRef -> EvalContext Axes
adimToAxes shape ref = do
  adim <- getAdimByAdimRef shape ref
  getAxes ref adim

adimsToAxes :: AbstractShape -> [AdimRef] -> EvalContext Axes
adimsToAxes shape = fmap mconcat . traverse (adimToAxes shape)

getIndicesFromParams :: Params -> EvalContext Indices
getIndicesFromParams params = fmap mconcat $ traverse (uncurry getAxisMapLike) $ HM.toList params

getSizesFromParams :: Params -> EvalContext Sizes
getSizesFromParams params = fmap mconcat $ traverse (uncurry getAxisMapLike) $ HM.toList params

eval :: Expr -> EvalContext (ErrorEnv Tensor)
eval expr = do
  state <- get
  case HM.lookup (exprId expr) $ evaluated state of
    Just t -> return t
    Nothing -> do
      t <- eval' expr
      modify' $ \s -> s {evaluated = HM.insert (exprId expr) t $ evaluated s}
      return t

tensorShapeToSizes :: TensorShape -> EvalContext Sizes
tensorShapeToSizes shape =
  fmap mconcat $
    traverse (uncurry getAxisMapLike) $
      labelledList ++ unlabelledList
  where
    labelledList =
      fmap (\(label, (_, map)) -> (ByLabel label, map)) $
        HM.toList $
          labelled shape
    unlabelledList = fmap (first ByAdim) $ HM.toList $ unlabelled shape

exprShape :: Expr -> EvalContext AbstractShape
exprShape expr = do
  state <- get
  return $ exprShapes state HM.! exprId expr

data SymIdentInfo = SymTensor TensorIdentifier | SymMap AdimIdentifier Int MapIdentifier
  deriving (Eq, Ord, Hashable, Generic, Lift, NFData)

instance Show SymIdentInfo where
  show (SymTensor ident) = show ident
  show (SymMap adim int map) = show adim <> "." <> show int <> "." <> show map

eval' :: Expr -> EvalContext (ErrorEnv Tensor)
eval' (Var _ ident) = do
  shape <- gets $ (HM.! ident) . allTensorShapes
  dtype <- gets $ (HM.! ident) . allTensorDTypes
  sizes <- tensorShapeToSizes shape
  return $ createTensor (withInfo "tensor" $ SymTensor ident) sizes dtype
eval' (NumBinOp _ op lhs rhs) = do
  l <- eval lhs
  r <- eval rhs
  return $ numBinOp op l r
eval' (NumScalarBinOp _ op lhs rhs) = do
  l <- eval lhs
  return $ numScalarBinOp op l rhs
eval' (NumUnaryOp _ op expr) = do
  e <- eval expr
  return $ numUnaryOp op e
eval' (BoolBinOp _ op lhs rhs) = do
  l <- eval lhs
  r <- eval rhs
  return $ boolBinOp op l r
eval' (BoolScalarBinOp _ op lhs rhs) = do
  l <- eval lhs
  return $ boolScalarBinOp op l $ BoolElem $ TensorElemVal rhs
eval' (BoolUnaryOp _ op expr) = do
  e <- eval expr
  return $ boolUnaryOp op e
eval' (CompareOp _ op lhs rhs) = do
  l <- eval lhs
  r <- eval rhs
  return $ compareOp op l r
eval' (Reduce _ expr abstractSI) = do
  e <- eval expr
  si <- getIndicesFromParams abstractSI
  return $ reduce e si
eval' (Broadcast _ expr extendedShape) = do
  e <- eval expr
  sizes <- tensorShapeToSizes extendedShape
  return $ broadcast e sizes
eval' (Constant _ elem shape) = do
  sizes <- tensorShapeToSizes shape
  return $ constantTensor elem sizes
eval' (Iota _ shape ref) = do
  sizes <- tensorShapeToSizes shape
  adim <- getAdimByAdimRef (toAbstractShape shape) ref
  axes <- getAxes ref adim
  if HS.size axes /= 1
    then error "Iota: adim must have a single axis"
    else return $ iota sizes (head $ HS.toList axes)
eval' (Slice _ expr SliceArgsExpr {..}) = do
  e <- eval expr
  start <- getIndicesFromParams start
  end <- getIndicesFromParams end
  strides <- getIndicesFromParams strides
  return $ slice e $ SliceArgs {..}
eval' (Pad _ expr elem PaddingArgsExpr {..}) = do
  e <- eval expr
  l <- getSizesFromParams low
  h <- getSizesFromParams high
  i <- getSizesFromParams interior
  return $ pad e elem $ PaddingArgs {padLow = l, padInterior = i, padHigh = h}
eval' (DynamicSlice _ expr DySliceArgsExpr {..}) = do
  e <- eval expr
  start <- getIndicesFromParams start
  sizes <- getSizesFromParams sizes
  return $ dynamicSlice e $ DySliceArgs {..}
eval' (DynamicUpdateSlice _ expr update start) = do
  e <- eval expr
  u <- eval update
  s <- getIndicesFromParams start
  return $ dynamicUpdateSlice e u s
eval' (Concat _ lhs rhs ref) = do
  l <- eval lhs
  r <- eval rhs
  lShape <- gets $ (HM.! exprId lhs) . exprShapes
  adim <- getAdimByAdimRef lShape ref
  axes <- getAxes ref adim
  if HS.size axes /= 1
    then error "Concat: adim must have a single axis"
    else return $ concatTensor l r (head $ HS.toList axes)
eval' (ConcatList _ exprs ref) = do
  es <- traverse eval exprs
  headShape <- gets $ (HM.! exprId (head exprs)) . exprShapes
  adim <- getAdimByAdimRef headShape ref
  axes <- getAxes ref adim
  if HS.size axes /= 1
    then error "ConcatList: adim must have a single axis"
    else return $ concatTensorList es (head $ HS.toList axes)
eval' (Relabel _ expr map) = do
  e <- eval expr
  originalShape <- exprShape expr
  loweredRelabelMap <-
    mconcat
      <$> mapM (uncurry $ mapEntryToAxesMap originalShape) (HM.toList map)
  return $ relabel e loweredRelabelMap
  where
    mapEntryToAxesMap originalShape from to = do
      fromAdim <- getAdimByAdimRef originalShape from
      fromAxesList <- getAxesList from fromAdim
      toAxesList <- getAxesList to fromAdim
      return $ HM.fromList $ zip fromAxesList toAxesList
eval' (Dot _ lhs rhs contractingSIMaps batch) = do
  l <- eval lhs
  r <- eval rhs
  lhsShape <- exprShape lhs
  contractingSI <- getIndicesFromParams contractingSIMaps
  batchAxes <- adimsToAxes lhsShape batch
  return $ dot l r contractingSI batchAxes
eval'
  ( ConvBase
      _
      input
      weight
      ConvConfigArgsExpr {..}
    ) = do
    i <- eval input
    w <- eval weight
    lhsShape <- exprShape input
    rhsShape <- exprShape weight
    batchAxes <- adimsToAxes lhsShape batchAdims
    featureAxes <- adimsToAxes lhsShape featureAdims
    outputFeatureAxes <- adimsToAxes rhsShape outputFeatureAdims
    s <- getIndicesFromParams strides
    si <- getIndicesFromParams contractingSIMaps
    return $
      convBase i w $
        ConvConfigArgs
          { convBatchAxes = batchAxes,
            convFeatureAxes = featureAxes,
            convOutputFeatureAxes = outputFeatureAxes,
            convStrides = s,
            convContractingSIMap = si
          }
eval'
  ( Conv
      _
      input
      weight
      ConvConfigArgsExpr {..}
      ConvPaddingArgsExpr {..}
    ) = do
    i <- eval input
    w <- eval weight
    lhsShape <- exprShape input
    rhsShape <- exprShape weight
    batchAxes <- adimsToAxes lhsShape batchAdims
    featureAxes <- adimsToAxes lhsShape featureAdims
    outputFeatureAxes <- adimsToAxes rhsShape outputFeatureAdims
    s <- getIndicesFromParams strides
    si <- getIndicesFromParams contractingSIMaps
    l <- getSizesFromParams low
    ld <- getSizesFromParams ldilation
    h <- getSizesFromParams high
    rd <- getSizesFromParams rdilation
    return $
      conv
        i
        w
        ConvConfigArgs
          { convBatchAxes = batchAxes,
            convFeatureAxes = featureAxes,
            convOutputFeatureAxes = outputFeatureAxes,
            convStrides = s,
            convContractingSIMap = si
          }
        ConvPaddingArgs
          { convLowPadding = l,
            convLDilation = ld,
            convHighPadding = h,
            convRDilation = rd
          }
eval' (Clamp _ emin' e' emax') = do
  emin <- eval emin'
  e <- eval e'
  emax <- eval emax'
  return $ clamp emin e emax
eval' (ClampScalar _ imin' e' imax') = do
  e <- eval e'
  return $ clampScalar imin' e imax'
eval' (Select _ c' t' e') = do
  c <- eval c'
  t <- eval t'
  e <- eval e'
  return $ select c t e
eval' (ReverseTensor _ expr axes) = do
  shape <- exprShape expr
  reverseAxes <- adimsToAxes shape axes
  e <- eval expr
  return $ reverseTensor e reverseAxes
eval' res@(ReshapeDegenerate _ expr introAxes elimAxes) = do
  e <- eval expr
  shape <- exprShape expr
  resShape <- exprShape res
  introAxes' <- adimsToAxes resShape (fst <$> introAxes)
  elimAxes' <- adimsToAxes shape elimAxes
  return $ reshapeDegenerate e introAxes' elimAxes'

evalRewrite :: Rewrite -> EvalContext (ErrorEnv Tensor, ErrorEnv Tensor)
evalRewrite (Rewrite _ lhs rhs) = do
  l <- eval lhs
  r <- eval rhs
  return (l, r)
