module Rerefined.Predicate.Relational.Length where

import Rerefined.Predicate.Common
import Rerefined.Predicate.Relational.Internal
import GHC.TypeNats ( Natural, KnownNat, natVal' )
import Data.MonoTraversable ( MonoFoldable(olength) )
import GHC.Exts ( Proxy# )

import Rerefined.Refined
import GHC.Exts ( coerce )
import GHC.TypeError
import Data.Kind ( type Constraint )

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

-- TODO improve type error here
widenCompareLength
    :: forall m op n a
    .  WROE op n m
    => Refined (CompareLength op n) a
    -> Refined (CompareLength op m) a
widenCompareLength = coerce

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
