-- | 'Refined' definition.
--
-- Not intended for external use. For unsafe refines, use
-- 'Rerefined.Refine.Unsafe'.

module Rerefined.Refined where

import Language.Haskell.TH.Syntax ( Lift )

-- | @a@ refined with predicate @p@.
newtype Refined p a = Refined a
    deriving stock (Lift, Show) -- TODO Show? useful but meh?

-- | Strip the refinement from a 'Refined'.
--
-- This is kept as a separate function for prettier @'Show' 'Refined'@ output.
unrefine :: Refined p a -> a
unrefine (Refined a) = a
