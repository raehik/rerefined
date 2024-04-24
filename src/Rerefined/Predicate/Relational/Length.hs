module Rerefined.Predicate.Relational.Length where

import Rerefined.Predicate.Common
import Rerefined.Predicate.Relational.Internal
import GHC.TypeNats ( Natural, KnownNat, natVal' )
import Data.MonoTraversable ( MonoFoldable(olength) )
import GHC.Exts ( Proxy# )

-- | Compare length to a type-level 'Natural' using the given 'RelOp'.
data CompareLength (op :: RelOp) (n :: Natural)
    deriving Predicate via Typeably (CompareLength op n)

-- | Compare the length of a 'Foldable' to a type-level 'Natural' using the
--   given 'RelOp'.
instance (KnownNat n, Foldable f, ReifyRelOp op)
  => Refine1 (CompareLength op n) f where
    validate1 p = validateCompareLength p . length

-- | Compare the length of a 'MonoFoldable' to a type-level 'Natural' using the
--   given 'RelOp'.
instance (KnownNat n, MonoFoldable a, ReifyRelOp op)
  => Refine (CompareLength op n) a where
    validate p = validateCompareLength p . olength

validateCompareLength
    :: forall op n. (KnownNat n, ReifyRelOp op)
    => Proxy# (CompareLength op n) -> Int -> Maybe (RefineFailure String)
validateCompareLength p len =
    validateBool p ("length not "<>reifyRelOpPretty @op<>" "<>show n)
        (reifyRelOp @op len (fromIntegral n))
  where n = natVal' (proxy# @n)
