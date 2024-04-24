{-# LANGUAGE AllowAmbiguousTypes #-}

{- TODO

This is the "correct" approach, rather than the repetitive refined one. But
errors degrade (though I suppose we could write all the pretty predicate names
out manually xd). And huh, TypeReps always tick their constructors, even when
they don't need disambiguating. Makes sense in my head (no ctx), but too bad.
-}

module Rerefined.Predicate.Relational.Internal where

import Data.Typeable ( Typeable )

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
