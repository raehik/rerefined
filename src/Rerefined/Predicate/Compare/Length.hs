-- Look how easy this is when you think about your patterns a bit!!

module Rerefined.Predicate.Compare.Length where

import Rerefined.Predicate.Common
import Rerefined.Predicate.Compare
import GHC.TypeNats ( Natural, KnownNat, natVal' )

-- | Compare length to a type-level 'Natural' using the given 'Comparison'.
data CompareLength (op :: Comparison) (n :: Natural)
    deriving Predicate via Typeably (CompareLength op n)

instance (KnownNat n, Foldable f, CompareN op)
  => Refine1 (CompareLength op n) f where
    validate1 p fa =
        validateBool p ("size not "<>compareNPretty @op<>" "<>show n)
            (compareN @op (length fa) (fromIntegral n))
      where n = natVal' (proxy# @n)

instance (KnownNat n, Foldable f, CompareN op)
  => Refine (CompareLength op n) (f a) where
    validate = validate1
