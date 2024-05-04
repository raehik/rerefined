-- | 'Refined' and 'Refined1' definitions for refined values.
--
-- Not intended for external use. For unsafe refining, use
-- 'Rerefined.Refine.Unsafe'.

module Rerefined.Refined
  (
  -- * @Refined@
    Refined(..)
  , unrefine

  -- * @Refined1@
  , Refined1(..)
  , unrefine1
  ) where

import Language.Haskell.TH.Syntax ( Lift )

-- | @a@ refined with predicate @p@.
newtype Refined p a = Refined a
    deriving stock (Lift, Show) -- TODO Show? useful but meh?

-- | Strip the refinement from a 'Refined'.
--
-- This is kept as a separate function for prettier @'Show' 'Refined'@ output.
unrefine :: Refined p a -> a
unrefine (Refined a) = a

-- | @f a@ refined with predicate @p@.
newtype Refined1 p f a = Refined1 (f a)
    deriving stock (Functor, Lift, Show) -- TODO Show? useful but meh?

-- | Strip the refinement from a 'Refined1'.
--
-- This is kept as a separate function for prettier @'Show' 'Refined1'@ output.
unrefine1 :: Refined1 p f a -> f a
unrefine1 (Refined1 fa) = fa
