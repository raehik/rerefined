{-# LANGUAGE AllowAmbiguousTypes #-} -- for CompareBOpN

module Rerefined.Predicate.Compare.Natural where

import Rerefined.Predicate.Common
import Rerefined.Predicate.Compare
import GHC.TypeNats ( Natural, KnownNat, natVal' )

-- | Compare to a type-level 'Natural' using the given 'CompareBOp'.
data CompareNatural (op :: CompareBOp) (n :: Natural)
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

type LessThan n = CompareNatural CBOpLT n
instance (KnownNat n, Num a, Ord a, CompareBOpN op)
  => Refine (CompareNatural op n) a where
    validate p a =
        -- TODO do I want to have n as arg0, so that the op is partially
        -- applied? or more likely, does it not matter in the slightest thanks
        -- to GHC's inlining?
        validateBool p ("not "<>compareBOpNPretty @op<>" "<>show n)
            (compareBOpN @op a(fromIntegral n))
      where n = natVal' (proxy# @n)

-- | Utilities for using @op@ as a numeric term-level operator.
--
-- We stuff the 'Typeable' constraint in here because we need it for the generic
-- 'Refine' instance, and we don't want to expose the 'Typeable' constraint.
-- It's guaranteed to exist, GHC just can't prove it.
class Typeable op => CompareBOpN (op :: CompareBOp) where
    -- | The term-level numeric comparison binary operator for @op@.
    compareBOpN :: forall a. (Num a, Ord a) => a -> a -> Bool

    -- | Pretty operator.
    compareBOpNPretty :: String

instance CompareBOpN CBOpGT where
    compareBOpN = (>)
    compareBOpNPretty = ">"

instance CompareBOpN CBOpGTE where
    compareBOpN = (>=)
    compareBOpNPretty = ">="

instance CompareBOpN CBOpEQ where
    compareBOpN = (==)
    compareBOpNPretty = "=="

instance CompareBOpN CBOpLTE where
    compareBOpN = (<=)
    compareBOpNPretty = "<="

instance CompareBOpN CBOpLT where
    compareBOpN = (<)
    compareBOpNPretty = "<"
