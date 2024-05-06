{-# LANGUAGE AllowAmbiguousTypes #-}  -- for ReifyRelOp
{-# LANGUAGE UndecidableInstances #-} -- for WidenRelOp

{- | Relational operator definitions.

Haskell base exports the type 'Ordering', which is an enum that states the
result of comparing two 'Ord's. We can utilize this to define /relational
operators/:

* 'LT', 'EQ' and 'GT' already map to relational operators.
* The others can be defined by combining the above with 'Or'. e.g. @'LT' ``Or``
  'EQ'@ -> "less than or equal" ('<=')

What's the point? We save on definitions, and get to reuse well-known data types
which most users will have intuition for. We do have to contest with
commutativity, but this is an extremely minor concern which can only come up if
you don't use the provided type synonyms, or do lots of type-level predicate
manipulation. And we provide those swapped-order instances anyway!
-}

module Rerefined.Predicate.Relational.Internal where

import Rerefined.Predicate.Logical.Or
import GHC.TypeNats
import Data.Type.Ord ( OrdCond )
import GHC.TypeLits ( Symbol )

import Data.Kind ( Type )

type LTE = LT `Or` EQ

-- | "not equal to" is equivalent to "strictly less than or greater than". We
--   could use 'Rerefined.Predicate.Logical.Not.Not', but sticking with just
--   'Or' keeps the internals simple.
type NEQ = LT `Or` GT

type GTE = GT `Or` EQ

-- | Reify a relational operator type tag.
--
-- Permitted operators are @Ordering@ constructors 'LT', 'EQ' and 'GT'; and
-- combinations of these using 'Or'.
class ReifyRelOp op where
    -- | Pretty @op@.
    type ShowRelOp op :: Symbol

    -- | The term-level relational operator that @op@ describes.
    reifyRelOp :: forall a. Ord a => a -> a -> Bool

instance ReifyRelOp LT where
    type ShowRelOp LT = "<"
    reifyRelOp = (<)

instance ReifyRelOp LTE where
    type  ShowRelOp LTE = "<="
    reifyRelOp = (<=)

-- | Hidden instance. You won't see this if you use the type synonyms.
deriving via LTE instance ReifyRelOp (EQ `Or` LT)

instance ReifyRelOp EQ where
    type  ShowRelOp EQ = "=="
    reifyRelOp = (==)

instance ReifyRelOp NEQ where
    type  ShowRelOp NEQ = "/="
    reifyRelOp = (/=)

-- | Hidden instance. You won't see this if you use the type synonyms.
deriving via NEQ instance ReifyRelOp (GT `Or` LT)

instance ReifyRelOp GTE where
    type ShowRelOp GTE = ">="
    reifyRelOp = (>=)

-- | Hidden instance. You won't see this if you use the type synonyms.
deriving via GTE instance ReifyRelOp (EQ `Or` GT)

instance ReifyRelOp GT where
    type ShowRelOp GT = ">"
    reifyRelOp = (>)

-- | Can we widen the given 'RelOp' on the given 'Natural' from @n@ to @m@?
type WidenRelOp :: k -> Natural -> Natural -> Bool
type family WidenRelOp op n m where
    -- @n == m@? no problem
    WidenRelOp op  n n = True

    -- I'd love to simplify this, but 'CmpNat' is opaque.
    WidenRelOp LT  n m = OrdCond (CmpNat n m) True  True False
    WidenRelOp LTE n m = OrdCond (CmpNat n m) True  True False
    WidenRelOp GTE n m = OrdCond (CmpNat n m) False True True
    WidenRelOp GT  n m = OrdCond (CmpNat n m) False True True

    -- | swapped LTE, lower down in equation list because less common
    WidenRelOp (EQ `Or` LT) n m =
        OrdCond (CmpNat n m) True  True False

    -- | swapped GTE, lower down in equation list because less common
    WidenRelOp (EQ `Or` GT) n m =
        OrdCond (CmpNat n m) False True True

    -- can't widen (==) or (/=)
    WidenRelOp _   _ _ = False

-- this gets clumsier due to kinding clashes (k vs. Ordering)
type NormalizeOrRelOp :: Type -> Type
type family NormalizeOrRelOp op where
    NormalizeOrRelOp (EQ `Or` LT) = LTE
    NormalizeOrRelOp (GT `Or` LT) = NEQ
    NormalizeOrRelOp (EQ `Or` GT) = GTE
