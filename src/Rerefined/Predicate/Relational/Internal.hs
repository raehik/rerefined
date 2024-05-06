{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- for weird TODO stuff

module Rerefined.Predicate.Relational.Internal where

import Rerefined.Predicate.Logical.Or
import Rerefined.Predicate.Logical.Not
import GHC.TypeNats
import Data.Type.Ord ( OrdCond )
import GHC.TypeLits ( Symbol )

type LTE = LT `Or` EQ
type NEQ = Not EQ
type GTE = GT `Or` EQ

-- | Reify a relational operator type tag. TODO
class ReifyRelOp op where
    -- | Pretty @op@.
    type ShowRelOp op :: Symbol

    -- | The term-level relational operator that @op@ describes.
    reifyRelOp :: forall a. Ord a => a -> a -> Bool

instance ReifyRelOp LT where
    type ShowRelOp LT = "<"
    reifyRelOp = (<)

instance ReifyRelOp LTE where
    type ShowRelOp LTE = "<="
    reifyRelOp = (<=)

instance ReifyRelOp EQ where
    type ShowRelOp EQ = "=="
    reifyRelOp = (==)

instance ReifyRelOp NEQ where
    type ShowRelOp NEQ = "/="
    reifyRelOp = (/=)

instance ReifyRelOp GTE where
    type ShowRelOp GTE = ">="
    reifyRelOp = (>=)

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

    -- can't widen (==) or (/=)
    WidenRelOp _   _ _ = False
