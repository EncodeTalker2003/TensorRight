{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module TensorRight
  ( -- * Language contructs

    -- ** @RClasses@

    -- | An @RClass@ is set of individual axes (where each refers to a single,
    -- concrete axis in a tensor). Every @RClass@ could comprise of an arbitrary
    -- number of individual-axes, and this set is kept uninterpreted/symbolic.
    -- Our language does not allow splitting of an @RClass@ into smaller @RClasses@,
    -- or adding individual-axes to an @RClass@. This means that to operate on an
    -- @RClass@, the user needs to perform the same operation on every
    -- individual-axis in it.
    --
    -- We can use 'newRClass' and 'newRClasses' to declare @RClasses@ in our language:

    -- | @
    --   rclass <- 'newRClass' "rclass"
    --   [rclass1, rclass2] <- 'newRClasses' ["rclass0", "rclass1"]
    -- @

    -- | This is written using Haskell's @do@ notation. 'newRClass' takes an RClass
    -- name and return an RClass identifier corresponding to it, while 'newRClasses'
    -- takes a list of names, and returns a list of @RClass@ identifiers,
    -- corresponding to those name. The returned @RClass@ identifiers can be used
    -- further in the code.

    -- ** Maps

    -- | As seen before, an @RClass@ is a symbolic set of individual-axes. If we
    -- want to represent the sizes of individual-axes in an @RClass@, we can use a
    -- map from those individual-axes, to their respective size or indices.
    -- Since any RClass is symbolic, and is represented by an identifier, these
    -- maps would also be symbolic and can be represented by an identifier
    --
    -- We can use the 'newMap' and 'newMaps' functions to declare maps on a
    -- given rclass in our language:

    -- | @
    -- map <- 'newMap' "map" rclass
    -- [map1, map2] <- 'newMaps' ["map1, map2"] rclass
    -- @

    -- | 'newMap' takes a Map name and an @RClass@ identifier, and returns a @Map@
    -- identifier, while 'newMaps' takes a list of names and returns a list of
    -- @Map@ identifiers, on the given @RClass@. A @Map@ identifier represents a
    -- symbolic map, whose domain is the same as the individual-axes in an RClass.
    -- So, we can say in this example, that @dom(map) = rclass@.
    --
    -- The same map structure can also be used to represent operator parameters
    -- like slice, pad configurations, etc.

    -- *** Syntax sugars for creating maps

    -- | Sometimes, we want to create a map with some constraints, like all the
    -- elements in the map should be non-negative, or all the elements should be
    -- constant. This can be achieved with preconditions (see below), but it is
    -- so common and using preconditions can be verbose. So, we provide some
    -- syntax sugars for creating such maps:

    -- | @
    -- [nonNeg] <- 'newNonNegMap' "nonNeg" rclass
    -- [constMap] <- 'newConstMap' "constMap" 1 rclass
    -- @

    -- | In the code, the first line creates a map named @nonNeg@ on the given
    -- @RClass@, with the constraint that all the elements in the map should be
    -- non-negative. The second line creates a map named @constMap@ on the given
    -- rclass with all the elements in the map being constant 1.

    -- | We may also combine multiple maps into a single map, using the
    -- 'combineMap' call:

    -- | @
    -- summedMap <- 'combineMap' "summedMap" 'sum' [map1, map2]
    -- -- 'sumMap' is equivalent to 'combineMap' with 'sum' as the combining
    -- -- function
    -- summedMap <- 'sumMap' "summedMap" [map1, map2]
    -- @

    -- ** Tensor creation

    -- | To describe a tensor, we need to describe its shape first. A tensor
    -- comprises of a set of @RClasses@, possibly duplicated, where each @RClass@ has
    -- its own set of individual axes. A tensor's shape is supposed to tell the
    -- size of the tensor along every axis. We can then describe the shape of a
    -- tensor as follows:
    --
    -- * For each @RClass@, we can use a (symbolic) map, or a @Map@ identifier, to
    -- represent sizes of the individual axes in that @RClass@
    -- * The shape of tensor can be represented as a nested map from @RClass@
    -- identifiers to @Map@ identifiers.
    --
    -- For example, we can create a tensor having two @RClasses@ @rclass0@ and
    -- @rclass0@ as follows:

    -- | @
    --
    -- [rclass0, rclass1] <- 'newRClasses' ["rclass0", "rclass1"]
    -- size0 <- 'newMap' "size0" rclass0
    -- size1 <- 'newMap' "size1" rclass1
    --
    -- let tensorShape = [rclass0 '-->' size0, rclass1 '-->' size1]
    --
    -- tensor <- 'newTensor' "tensor" 'IntType' tensorShape
    -- @

    -- | We first declared two @RClass@ identifiers @rclass0@ and @rclass1@. Then, we
    -- declared @Map@ identifiers @size0@ and @size2@ on @rclass0@ and @rclass1@
    -- respectively. Then @tensorShape@ represents the shape of a tensor with 2
    -- @RClasses@, and the specified sizes. @rclass0 '-->' size0@ represents that
    -- @size0@ contains the sizes of individual-axes in @rclass0@.
    --
    -- We then created a tensor using 'newTensor', which takes a name for the
    -- tensor, the base element type (in this case 'IntType') and the tensor
    -- shape. 'newTensor' returns a @Tensor@ Identifier, which can be used to
    -- refer to this tensor in expressions. The created tensor is said to
    -- contain symbolic integer elements, and is of shape @tensorShape@.

    -- *** Duplicate RClasses

    -- | We could have multiple copies of the same @RClass@ in a tensor's shape. For
    -- example, consider a tensor with 2 copies of @rclass0@

    -- | @
    -- rclass0 <- 'newRClass' "rclass0"
    -- [size, size'] <- 'newMaps' ["size", "size'"] rclass0
    -- let tensorShape = [rclass0 '-->' size, rclass0 '-->' size']
    -- tensor <- 'newTensor' "tensor" IntType tensorShape
    -- @

    -- | In such a case, if @rclass0@ had \(k\) dimesions, then the tensor has
    -- \(2k\) dimensions. Let's say we wanted to operate on this tensor, like
    -- reduce on one specific copy of @rclass0@.

    -- | @
    -- out <- 'reduce' tensor rclass0
    -- @

    -- | It is unclear in this expression, which copy of @rclass0@ gets reduced
    -- out -- is it the one with sizes @size@ or @size'@? To mitigate this
    -- issue, we allow the user to diambiguate duplicate @RClasses@ with labels,
    -- and use those labels to refer to a specific @RClass@. We use the '@@'
    -- syntax to specify labels as follows. Labels are just strings.

    -- | @
    -- let tensorShape = [rclass0 '-->' size '@@' "label0", rclass0 '-->' size' '@@' "label1"]
    -- tensor <- 'newTensor' "tensor" 'IntType' tensorShape
    -- out <- 'reduce' tensor (ByLabel "label1")
    -- @

    -- | Now, this expression is unambiguous and we know what @RClass@ to reduce
    -- on.
    --
    -- Note that, in a tensor's shapes, if an @RClass@ identifier appears multiple
    -- times, then all copies need to have a disambigutating label. So,

    -- | @
    -- let tensorShapeValid =
    --       [
    --         rclass0 '-->' size '@@' "label0",
    --         rclass0 '-->' size' '@@' "label1",
    --         rclass0 '-->' size '@@' "label2"
    --       ]
    -- let tensorShapeInvalid =
    --       [
    --         rclass0 '-->' size '@@' "label0",
    --         rclass0 '-->' size' '@@'
    --       ]
    -- @

    -- | @tensorShapeValid@ is a valid shape, since all copies of @rclass0@ have a
    -- label, but the second shape is invalid. For the first shape, the user can
    -- refer to the @RClasses@ by @'ByLabel' "label0"@ for the first copy,
    -- (@'ByLabel' "label1"@) for the second copy, and (@'ByLabel' "label2"@)
    -- for the third copy.

    -- | The user can omit labels, in the case when no disambiguatation is needed.

    -- | @
    -- let tensorShapeValid =
    --       [
    --         rclass0 '-->' size0 '@@' "label0",
    --         rclass1 '-->' size1 '@@' "label1"
    --       ]
    -- let tensorShapeValid' =
    --       [
    --         rclass0 '-->' size0 '@@' "label0",
    --         rclass1 '-->' size1 '@@'
    --       ]
    -- @

    -- | Both of these shapes are valid, but the only difference is that we need
    -- to use the label @label1@ to refer to @rclass1@ in the first shape, while
    -- in the second shape, we need to refer to it by @rclass1@ itself. Note that
    -- there was no need to specify labels here, because there are no duplicate
    -- @RClasses@, but we allow the user the flexibility to use labels.

    -- ** Tensor expressions

    -- | We will now describe the tensor operations supported in our DSL. @Expr@
    -- represents the type of a tensor expression. Refer to
    -- <https://github.com/google-research/ml_for_ml_compilers/tree/siruilu-tr-hs/tensor_right/tr-hs/rules>
    -- to take a look at concrete examples on how to instantiate tensor
    -- expressions.

    -- | For example, you may construct a tensor expression as follows:

    -- | @
    -- rclass <- 'newRClass' "rclass"
    -- map <- 'newMap' "map" rclass
    -- tensor <- 'newTensor' "tensor" 'IntType' [rclass '-->' map]
    -- expr <- 'numBinOp' 'Add' tensor tensor
    -- @

    -- | For the detailed list of operations supported, refer to the
    -- [operations]("TensorRight#g:op") section.

    -- ** Rewrite rules

    -- | A Rewrite rule consists of
    --
    -- * An LHS Tensor Expression
    -- * An RHS Tensor Expression
    -- * A set of preconditions (optional)
    -- * A set of si-relations (optional)
    --
    -- We saw in the previous section, how we can construct tensor expressions.
    -- We will now look at contructs to specify preconditions, si-relations and
    -- rewrite rules.

    -- *** Preconditions

    -- | Preconditions are boolean (or symbolic boolean) valued conditions,
    -- under which the rewrite rule is supposedly correct. We can specify
    -- preconditions using the 'precondition' function. Its syntax is

    -- | @
    -- 'precondition' [m1, m2, ...] f
    -- @

    -- | where
    --
    -- * @[m1, m2, ...]@ is a list of Map identifiers, all previously declared
    --   beforehand. It contains all the maps over which the precondition is
    --   defined
    -- * @f :: [Map] -> SymBool@ is a function, which takes a list of maps, and
    --   returns a symbolic boolean, denoting the precondition value. The list
    --   provided as input to f, has to be of the same size as the list of @Map@
    --   Identifiers
    -- * Note that in our DSL, all the @Map@ identifiers are symbolic, so they
    --   don't have concrete keys and values. But later, we infer how many
    --   individual axes are sufficient for every map, instantiate them with a
    --   fixed number of concrete-keys, and symbolic values (in this case,
    --   'Grisette.SymInteger'). The function f works directly with these instantiated
    --   maps
    --
    -- For example, we can write a precondition expressing that a maps needs to
    -- have all values as 0:

    -- | @
    -- rclass <- 'newRClass' "rclass"
    -- [size, padSize] <- 'newMap' ["size", "padSize"] rclass
    -- tensor <- 'newTensor' "t" 'IntType' [rclass '-->' size]
    -- lhs <- 'pad' tensor ('intElem' 0) $
    --   v'Padding'
    --     { low = [rclass '-->' padSize],
    --       interior = [rclass '-->' padSize],
    --       high = [rclass '-->' padSize]
    --     }
    -- 'precondition' [padSize] $ \[padSize] -> 'unaryCond' (.== 0) padSize
    -- @

    -- | The precondition first takes a list, containing only the 'TensorRight.Internal.DSL.Condition.MapIdentifier'
    -- @padSize@. It then takes a function, whose only argument is a
    -- singleton list containing the padding sizes, and it checks it all values
    -- in the map are 0 or not. It returns a symbolic boolean corresponding to
    -- the same.
    --
    -- The user can add multiple preconditions by using multiple calls to
    -- 'precondition'. The semantics are that the final precondition is the
    -- conjunction of all the individual preconditions.
    --
    -- For the constructs available to the user to express preconditions, please
    -- refer to the [preconditions]("TensorRight#g:precond") section.
    --
    -- Apart from the helpers provided there, some Grisette operators are also
    -- useful, like 'Grisette..==', 'Grisette.symIte', etc.

    -- *** Si-relations

    -- | The syntax to add si-relations is similar to preconditions -- we just
    -- need to use 'siRelation' instead of 'precondition'. For example, this
    -- si-relation expresses that rclass0lhssi == rclass0rhssi:

    -- | @
    -- 'siRelation' [rclass0lhssi, rclass0rhssi] $
    --   \[rclass0lhssi, rclass0rhssi] ->
    --     'elementWiseCondition' (.==) rclass0lhssi rclass0rhssi
    -- @

    -- *** Rewrite rules

    -- | Once we have declared all @RClass@ identifiers, @Map@ identifiers,
    -- @Tensor@ identifiers, created expressions and added preconditions, we can
    -- use the v'Rewrite' constructor to create a rewrite rule:

    -- | @
    -- lhs <- ...
    -- rhs <- ...
    -- return $
    --   v'Rewrite' "lhs => rhs" lhs rhs
    -- @

    -- | Here is a full example, that creates a rewrite rule, and wraps it in
    -- the appropriate context.

    -- | @
    -- rule :: 'DSLContext' t'Rewrite'
    -- rule = do
    --   rclass <- 'newRClass' "rclass"
    --   map <- 'newMap' "map" rclass
    --
    --   tensor <- 'newTensor' "tensor" 'IntType' [rclass '-->' map]
    --
    --   constTensor1 <- 'constantInt' "a" [rclass '-->' map]
    --   constTensor2 <- 'constantInt' "b" [rclass '-->' map]
    --   lhs <- 'numBinOp' 'Add' ('numBinOp' 'Add' tensor constTensor1) constTensor2
    --   rhs <- 'numBinOp' 'Add' tensor ('numBinOp' 'Add' constTensor1 constTensor2)
    --
    --   return $
    --     v'Rewrite' "Add(Add(A, Const), Const2) ⇒ Add(A, Add(Const, Const2))" lhs rhs
    -- @

    -- | Once this rule is declared, we can verify it using 'verifyDSL'.

    -- | @
    -- main :: IO ()
    -- main = 'verifyDSL' rule
    -- @

    -- ** Verification and debugging

    -- | We can verify the correctness of the DSL code using the 'verifyDSL'
    -- function.
    --
    -- On failure, it will print various information. To help understand the
    -- failure better, we also provide a set of monitoring functions, which can
    -- be used to print the shapes of expressions, or the contents of maps, etc.
    -- on failure.
    --
    -- The following code monitors whether the lhs tensor is valid, and if valid
    -- monitors its shape. It also monitors the content of the @low@ map on
    -- failure.

    -- | @
    -- 'monitorExprOnFailure' "lhs" lhs
    -- 'monitorMapOnFailure' "low" (ByRClass spatial) low
    -- @

    -- * Rewriting rule context
    DSLContext,
    Rewrite (..),
    ValidElem,
    ValidNum,
    AnyDTypeRule,
    NumRule,
    rewrite,

    -- * Creation
    RClassRef (..),
    newRClass,
    newRClasses,
    newMap,
    newMaps,
    newNonNegMap,
    newNonNegMaps,
    newConstMap,
    newConstMaps,
    sumMap,
    combineMap,
    newTensor,
    DType (..),

    -- * Conditions #precond#

    -- | Apart from the helpers provided below, some Grisette operators are also
    -- useful, like 'Grisette..==', 'Grisette.symIte', etc.
    siRelation,
    checkSIMap,
    precondition,
    siRelation',
    precondition',
    numTensorAssumption,
    zipCondition,
    elemWiseCond,
    unaryCond,
    elemWiseArith,
    unaryArith,

    -- * Elements

    -- * Operations #op#
    ToElem (..),
    NumBinOp (..),
    numBinOp,
    numBinScalarOp,
    BoolBinOp (..),
    boolBinOp,
    boolBinScalarOp,
    CompareOp (..),
    compareOp,
    NumUnaryOp (..),
    numUnaryOp,
    BoolUnaryOp (..),
    boolUnaryOp,
    reduce,
    broadcast,
    constant,
    iota,
    Slice (..),
    slice,
    Padding (..),
    intElem,
    boolElem,
    pad,
    padLow,
    relabel,
    DySlice (..),
    dynamicSlice,
    dynamicUpdateSlice,
    concatTensor,
    concatTensorList,
    dot,
    ConvConfig (..),
    ConvPadding (..),
    convBase,
    conv,
    clamp,
    clampScalar,
    select,
    reverseTensor,
    reshapeDegenerate,

    -- * Monitoring
    monitorExprOnFailure,
    monitorMapOnFailure,

    -- * Verification
    verifyDSL,
    verifyDSLWith,
    verifyAnyDTypeDSL,
    verifyAnyDTypeDSLWith,
    verifyNumDSL,
    verifyNumDSLWith,

    -- * Syntax
    ArrowSyntax (..),
    AtSyntax (..),

    -- * Tensor int
    TensorNum,
    TensorReal,
    TensorInt,
    nonInf,
    posInf,
    negInf,
  )
where

import TensorRight.Internal.Core.Tensor (ToElem (..))
import TensorRight.Internal.Core.Tensor.TensorInt
import TensorRight.Internal.DSL.Condition
import TensorRight.Internal.DSL.DSL
import TensorRight.Internal.DSL.Expr
import TensorRight.Internal.DSL.Syntax
import TensorRight.Internal.DSL.Verify
