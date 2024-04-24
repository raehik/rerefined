{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- for weird TODO stuff

module Rerefined.Predicate.Relational.Internal where

import Data.Typeable ( Typeable )

import GHC.TypeNats
import Data.Type.Ord ( Compare, OrdCond )

-- | Relational operator.
--
-- There are three possible outcomes from 'compare'ing two terms, defined in
-- 'Ordering'. However, we may instead compare terms using relational operators
-- such as @>=@, which are more specific comparisons that return a 'Bool'.
--
-- Constructor order is arbitrary due to @NEQ@, which obstructs ordering in a
-- meaningful way.
data RelOp
  = LT' -- ^ '<'  less than
  | LTE -- ^ '<=' less than or equal to
  | EQ' -- ^ '=='              equal to
  | NEQ -- ^ '/=' less than or             greater than
  | GTE -- ^ '>='              equal to or greater than
  | GT' -- ^ '>'                           greater than

-- | Reify a relational operator type tag.
--
-- We stuff the 'Typeable' constraint in here because we need it for easy
-- 'Rerefined.Predicate.Predicate' instances, and we don't want to expose the
-- 'Typeable' constraint elsewhere.
class Typeable op => ReifyRelOp (op :: RelOp) where
    -- | The term-level relational operator that @op@ describes.
    reifyRelOp :: forall a. (Num a, Ord a) => a -> a -> Bool

    -- | Pretty operator.
    reifyRelOpPretty :: String

instance ReifyRelOp LT' where
    reifyRelOp = (<)
    reifyRelOpPretty = "<"

instance ReifyRelOp LTE where
    reifyRelOp = (<=)
    reifyRelOpPretty = "<="

instance ReifyRelOp EQ' where
    reifyRelOp = (==)
    reifyRelOpPretty = "=="

instance ReifyRelOp NEQ where
    reifyRelOp = (/=)
    reifyRelOpPretty = "/="

instance ReifyRelOp GTE where
    reifyRelOp = (>=)
    reifyRelOpPretty = ">="

instance ReifyRelOp GT' where
    reifyRelOp = (>)
    reifyRelOpPretty = ">"

-- | Can we widen the given 'RelOp' from @n@ to @m@?
type family WidenRelOp (op :: RelOp) (n :: Natural) (m :: Natural) where
    WidenRelOp LT' n m = OrdCond (Compare n m) True  False False
    WidenRelOp LTE n m = OrdCond (Compare n m) True  True  False
    WidenRelOp EQ' n m = OrdCond (Compare n m) False True  False
    WidenRelOp NEQ n m = OrdCond (Compare n m) True  False True
    WidenRelOp GT' n m = OrdCond (Compare n m) False False True
    WidenRelOp GTE n m = OrdCond (Compare n m) False True  True
