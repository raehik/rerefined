{- | Unsafe refining.

Sometimes, you know that your value satisfies some predicate before validating.
For those cases, we permit skipping validation and obtaining a refined value
"for free".

You should be certain that your value cannot possibly fail the predicate you are
skipping. A good practice is to annotate all call sites with an explanation of
why the usage is safe.
-}

module Rerefined.Refine.Unsafe
  (
  -- * @Refined@
    type Refined
  , unsafeRefine
  , unsafeRerefine

  -- * @Refined1@
  , type Refined1
  , unsafeRefine1
  , unsafeRerefine1
  ) where

import Rerefined.Refined
import Rerefined.Refined1

-- | Construct a 'Refined' without validating the predicate @p@.
--
-- Unsafe. Use only when you can manually prove that the predicate holds.
unsafeRefine :: a -> Refined p a
unsafeRefine = Refined

-- | Replace a 'Refined''s predicate without validating the new prdicate @pNew@.
--
-- Unsafe. Use only when you can manually prove that the new predicate holds.
unsafeRerefine :: forall pNew pOld a. Refined pOld a -> Refined pNew a
unsafeRerefine = Refined . unrefine

-- | Construct a 'Refined1' without validating the predicate @p@.
--
-- Unsafe. Use only when you can manually prove that the predicate holds.
unsafeRefine1 :: f a -> Refined1 p f a
unsafeRefine1 = Refined1

-- | Replace a 'Refined1''s predicate without validating the new prdicate
--   @pNew@.
--
-- Unsafe. Use only when you can manually prove that the new predicate holds.
unsafeRerefine1 :: forall pNew pOld f a. Refined1 pOld f a -> Refined1 pNew f a
unsafeRerefine1 = Refined1 . unrefine1
