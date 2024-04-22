module Rerefined.Predicate.LessThan where

import Rerefined.Predicate.Common
import GHC.TypeNats ( Natural, KnownNat, natVal' )

data LessThan (n :: Natural) deriving Predicate via Typeably (LessThan n)
{-
instance KnownNat n => Predicate (LessThan n) where
    predicateName d = showParen (d > 10) $
        showString "LessThan " . showsPrec 11 (natVal' (proxy# :: Proxy# n))
-}

instance (KnownNat n, Num a, Ord a) => Refine (LessThan n) a where
    validate p a = validateBool p "not less than n" (a < fromIntegral n)
      where n = natVal' (proxy# @n)
