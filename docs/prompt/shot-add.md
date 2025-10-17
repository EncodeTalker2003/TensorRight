Here is an example of the work you need to do. Consider the following rewrite rule:

Add(Add(A, c1), c2) = Add(A, Add(c1, c2))

This rule says that: adding a constant to a sum can be done by adding the constant to one of the summands.

One version of the DSL code for it is:

```haskell
rule01 :: forall a. NumRule a
rule01 _ = do
  rclass <- newRClass "rclass"
  map <- newMap "map" rclass
  tA <- newTensor @a "A" [rclass --> map]
  c1 <- constant @a "c1" [rclass --> map]
  c2 <- constant @a "c2" [rclass --> map]
  lhs <- numBinOp Add (numBinOp Add tA c1) c2
  rhs <- numBinOp Add tA (numBinOp Add c1 c2)
  rewrite "Add(Add(A, c1), c2) ⇒ Add(A, Add(c1, c2))" lhs rhs

main :: IO ()
main = do
  verifyNumDSL rule01

```

