-- | 'Refined1' definition.
--
-- Not intended for external use. For unsafe refines, use
-- 'Rerefined.Refine.Unsafe'.

module Rerefined.Refined1 where

import Language.Haskell.TH.Syntax ( Lift )

-- | @f a@ refined with predicate @p@.
newtype Refined1 p f a = Refined1 (f a)
    deriving stock (Functor, Lift, Show) -- TODO Show? useful but meh?

-- | Strip the refinement from a 'Refined1'.
--
-- This is kept as a separate function for prettier @'Show' 'Refined1'@ output.
unrefine1 :: Refined1 p f a -> f a
unrefine1 (Refined1 fa) = fa
