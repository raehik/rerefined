module Rerefined.Predicate.Compare.Natural where

import Rerefined.Predicate.Common
import Rerefined.Predicate.Compare
import GHC.TypeNats ( Natural, KnownNat, natVal' )

-- | Compare to a type-level 'Natural' using the given 'CompareBOp'.
data CompareNatural (op :: Comparison) (n :: Natural)
    deriving Predicate via Typeably (CompareNatural op n)
-- TODO I could write custom predicateNames here if I wanted to override how
-- they display. But I don't mind the expanded type synonyms. @CompareNatural
-- 'CBOpLT 10@ still makes sense to me, especially with the extra message.
--
-- I should simplify op names, but not sure what to, since I can't use LT/EQ/GT.
{-
instance KnownNat n => Predicate (LessThan n) where
    predicateName d = showParen (d > 10) $
        showString "LessThan " . showsPrec 11 (natVal' (proxy# :: Proxy# n))
-}

type LessThan n = CompareNatural LT' n
instance (KnownNat n, Num a, Ord a, CompareN op)
  => Refine (CompareNatural op n) a where
    validate p a =
        -- TODO do I want to have n as arg0, so that the op is partially
        -- applied? or more likely, does it not matter in the slightest thanks
        -- to GHC's inlining?
        validateBool p ("not "<>compareNPretty @op<>" "<>show n)
            (compareN @op a(fromIntegral n))
      where n = natVal' (proxy# @n)
