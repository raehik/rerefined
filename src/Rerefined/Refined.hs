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
  , squashRefined1
  ) where

import Language.Haskell.TH.Syntax ( Lift )

-- | @a@ refined with predicate @p@.
newtype Refined p a = Refined a
    deriving stock (Lift, Eq, Ord, Show)

-- | Strip the refinement from a 'Refined'.
--
-- This is kept as a separate function for prettier @'Show' 'Refined'@ output.
unrefine :: Refined p a -> a
unrefine (Refined a) = a

-- | @f a@ refined with predicate @p@.
--
-- We may derive legal 'Functor', 'Traversable' instances for this as
-- 'Rerefined.Predicate.Refine1' guarantees that the predicate only applies to
-- the functor structure. That is, you _may_ alter a 'Refined1' without
-- re-asserting its predicate, provided your changes are made without altering
-- the structure/shape of @f@ (e.g. 'fmap', 'traverse').
newtype Refined1 p f a = Refined1 (f a)
    deriving stock (Functor, Foldable, Traversable, Lift, Eq, Ord, Show)

-- | Strip the refinement from a 'Refined1'.
--
-- This is kept as a separate function for prettier @'Show' 'Refined1'@ output.
unrefine1 :: Refined1 p f a -> f a
unrefine1 (Refined1 fa) = fa

-- | Squash a 'Refined1' into a 'Refined'. Essentially forget the @f@.
squashRefined1 :: Refined1 p f a -> Refined p (f a)
squashRefined1 = Refined . unrefine1
