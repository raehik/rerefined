{- TODO

This is the "correct" approach, rather than the repetitive refined one. But
errors degrade (though I suppose we could write all the pretty predicate names
out manually xd). And huh, TypeReps always tick their constructors, even when
they don't need disambiguating. Makes sense in my head (no ctx), but too bad.
-}

module Rerefined.Predicate.Compare where

-- | A binary comparison operation.
--
-- The "and equal" operators can be defined by composing 'EQ' with the
-- appropriate 'GT' or 'LT', and this would let us reuse 'Ordering', but reduces
-- efficiency e.g. GTE would check @>@ then @=@, rather than just checking @>=@.
data CompareBOp
    = CBOpGT  -- ^ @>@  greater than
    | CBOpGTE -- ^ @>=@ greater than or equal to
    | CBOpEQ  -- ^  @=@                 equal to
    | CBOpLTE -- ^ @<=@    less than or equal to
    | CBOpLT  -- ^ @<@     less than
