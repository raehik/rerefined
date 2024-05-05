{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-} -- for GHC <= 9.4, WROE

module Rerefined.Predicate.Relational.Length where

import Rerefined.Predicate.Common
import Rerefined.Predicate.Relational.Internal
import GHC.TypeNats ( Natural, KnownNat, natVal' )
import Data.MonoTraversable ( MonoFoldable(olength) )
import GHC.Exts ( Proxy# )

import Rerefined.Refine.Unsafe
import GHC.TypeError
import Data.Kind ( type Constraint )

-- | Compare length to a type-level 'Natural' using the given 'RelOp'.
data CompareLength (op :: RelOp) (n :: Natural)
instance Predicate (CompareLength op n) where
    type PredicateName d (CompareLength op n) = "TODO"

-- | Compare the length of a 'Foldable' to a type-level 'Natural' using the
--   given 'RelOp'.
instance
  ( KnownNat n, Foldable f, ReifyRelOp op
  , KnownSymbol (PredicateName 0 (CompareLength op n))
  ) => Refine1 (CompareLength op n) f where
    validate1 p = validateCompareLength p . length

-- | Compare the length of a 'MonoFoldable' to a type-level 'Natural' using the
--   given 'RelOp'.
instance
  ( KnownNat n, MonoFoldable a, ReifyRelOp op
  , KnownSymbol (PredicateName 0 (CompareLength op n))
  ) => Refine (CompareLength op n) a where
    validate p = validateCompareLength p . olength

validateCompareLength
    :: forall op n. (KnownNat n, ReifyRelOp op)
    => Proxy# (CompareLength op n) -> Int -> Maybe RefineFailure
validateCompareLength p len =
    validateBool p ("length not "<>reifyRelOpPretty @op<>" ") -- TODO <>show n)
        (reifyRelOp @op len (fromIntegral n))
  where n = natVal' (proxy# @n)

-- | Widen a length comparison predicate.
--
-- Only valid widenings are permitted, checked at compile time.
--
-- Example: Given a >= 1, we know also that a >= 0. Thus, this function allows
-- you to turn a @Refined (CompareLength GTE 1) a@ into a @Refined
-- (CompareLength GTE 0) a@.
--
-- TODO improve type error here
widenCompareLength
    :: forall m op n a
    .  WROE op n m
    => Refined (CompareLength op n) a
    -> Refined (CompareLength op m) a
widenCompareLength = unsafeRerefine

-- | Widen a length comparison predicate.
--
-- Only valid widenings are permitted, checked at compile time.
--
-- Example: Given a >= 1, we know also that a >= 0. Thus, this function allows
-- you to turn a @Refined1 (CompareLength GTE 1) f a@ into a @Refined1
-- (CompareLength GTE 0) f a@.
--
-- TODO improve type error here
widenCompareLength1
    :: forall m op n f a
    .  WROE op n m
    => Refined1 (CompareLength op n) f a
    -> Refined1 (CompareLength op m) f a
widenCompareLength1 = unsafeRerefine1

type WROE op n m = WROE' op n m (WidenRelOp op n m)
type WROE' :: RelOp -> Natural -> Natural -> Bool -> Constraint
type family WROE' (op :: RelOp) (n :: Natural) (m :: Natural) (b :: Bool) where
    WROE' op n m True  = ()
    WROE' op n m False = TypeError
      (      Text "can't widen relational equation "
        :$$: ShowType op :<>: Text " " :<>: ShowType n
        :$$: Text "to"
        :$$: ShowType op :<>: Text " " :<>: ShowType m
      )
