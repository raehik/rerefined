{-# LANGUAGE AllowAmbiguousTypes #-}

{- TODO

This is the "correct" approach, rather than the repetitive refined one. But
errors degrade (though I suppose we could write all the pretty predicate names
out manually xd). And huh, TypeReps always tick their constructors, even when
they don't need disambiguating. Makes sense in my head (no ctx), but too bad.
-}

module Rerefined.Predicate.Compare where

import Data.Typeable ( Typeable )

-- | Comparison operation type.
--
-- Note that we may define this type indirectly using 'Ordering', and composing
-- 'EQ' with 'GT' and 'LT' for the other two comparisons. Indeed, we have an
-- @Or@ predicate combinator, so it seems sensible. But it complicates things,
-- since we're now mixing @op :: 'CompareBOp'@ and @op :: 'Type'@, and we have
-- arbitrary associativity now. For ergonomics, we stick with this type.
data Comparison
  = GT' -- ^ @>@                           greater than
  | GTE -- ^ @>=@              equal to or greater than
  | EQ' -- ^ @==@              equal to
  | LTE -- ^ @<=@ less than or equal to
  | LT' -- ^ @<@  less than


-- | Utilities for using @op@ as a numeric term-level operator.
--
-- We stuff the 'Typeable' constraint in here because we need it for the generic
-- 'Refine' instance, and we don't want to expose the 'Typeable' constraint.
-- It's guaranteed to exist, GHC just can't prove it.
class Typeable op => CompareN (op :: Comparison) where
    -- | The term-level numeric comparison binary operator for @op@.
    compareN :: forall a. (Num a, Ord a) => a -> a -> Bool

    -- | Pretty operator.
    compareNPretty :: String

instance CompareN GT' where
    compareN = (>)
    compareNPretty = ">"

instance CompareN GTE where
    compareN = (>=)
    compareNPretty = ">="

instance CompareN EQ' where
    compareN = (==)
    compareNPretty = "=="

instance CompareN LTE where
    compareN = (<=)
    compareNPretty = "<="

instance CompareN LT' where
    compareN = (<)
    compareNPretty = "<"
