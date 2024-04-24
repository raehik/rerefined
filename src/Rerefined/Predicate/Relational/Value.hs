{-# LANGUAGE AllowAmbiguousTypes #-}

module Rerefined.Predicate.Relational.Value where

import Rerefined.Predicate.Common
import Rerefined.Predicate.Relational.Internal
import GHC.TypeNats ( Natural, KnownNat, natVal' )

-- | Compare value to a type-level 'Natural' using the given 'RelOp'.
data CompareValue (op :: RelOp) (sign :: Sign) (n :: Natural)
    deriving Predicate via Typeably (CompareValue op sign n)
-- TODO I could write custom predicateNames here if I wanted to override how
-- they display. But I don't mind the expanded type synonyms. @CompareValue
-- 'CBOpLT 10@ still makes sense to me, especially with the extra message.
--
-- I should simplify op names, but not sure what to, since I can't use LT/EQ/GT.
{-
instance KnownNat n => Predicate (LessThan n) where
    predicateName d = showParen (d > 10) $
        showString "LessThan " . showsPrec 11 (natVal' (proxy# :: Proxy# n))
-}

--type LessThan n = CompareValue LT' n

instance
  ( KnownNat n, Num a, Ord a
  , ReifyRelOp op, ReifySignedNat sign n, ReifySign sign
  ) => Refine (CompareValue op sign n) a where
    -- note that we show the reified 'Natural' rather than the coerced numeric
    -- type, as otherwise we'd need a @'Show' a@
    validate p a =
        validateBool p
            ("value not "<>reifyRelOpPretty @op<>" "<>signPretty @sign<>show n)
            (reifyRelOp @op a (reifySignedNat @sign @n))
      where n = natVal' (proxy# @n)

data Sign = Pos | Neg

class Typeable sign => ReifySign (sign :: Sign) where signPretty :: String
instance ReifySign Pos where signPretty = ""
instance ReifySign Neg where signPretty = "-"

-- TODO do I add any KnownNat constraints anywhere here
class ReifySignedNat (sign :: Sign) (n :: Natural) where
    reifySignedNat :: (Num a, KnownNat n) => a

instance ReifySignedNat Pos n where
    reifySignedNat = fromIntegral (natVal' (proxy# @n))

instance ReifySignedNat Neg n where
    reifySignedNat = negate (fromIntegral (natVal' (proxy# @n)))