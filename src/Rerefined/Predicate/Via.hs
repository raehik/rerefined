{-# LANGUAGE UndecidableInstances #-} -- for PredicateName
{-# LANGUAGE OverloadedStrings #-} -- for builder
{-# LANGUAGE AllowAmbiguousTypes #-} -- for validateVia

module Rerefined.Predicate.Via where

import Rerefined.Predicate.Common
import TypeLevelShow.Utils
import GHC.Exts ( Proxy# )

{- | Predicate @p@ is implemented via predicate @pVia@.

This is useful when you need a unique data type for a predicate (e.g. for
instances, clarity), but the predicate it tests already exists. In such cases,
it would be nice to reuse the existing predicate, while printing both predicates
in failures.

'Via' assists in doing this. Implement your 'Predicate' instance as normal, then
implement 'Refine' with:

@
'validate' = 'validateVia' \@pVia
@

Now when this predicate fails, it will print the @pVia@ failure with a wrapper
stating the name of the original predicate @p@.

Note that you may not use @DerivingVia@ because it only works on the last
parameter of a multi-parameter type class, and I don't want to switch the order
of the 'Refine' parameters. (Even if I did, @DerivingVia@ isn't much different
to or easier than this.)

This predicate should not be used directly. Doing so will confuse your failure
messages-- and accomplish very little, since you could simply be using @pVia@.
-}
data Via pVia p

instance (Predicate p, Predicate pVia)
  => Predicate (Via pVia p) where
    type PredicateName d (Via pVia p) = ShowParen (d > 9)
        ("Via " ++ PredicateName 10 p ++ " " ++ PredicateName 10 pVia)

instance (Refine pVia a, Predicate p, KnownPredicateName p)
  => Refine (Via pVia p) a where
    validate _p a =
        case validate (proxy# @pVia) a of
          Nothing -> Nothing
          Just e  -> validateFail (proxy# @p) "via" [e]

-- | Implement 'validate' for predicate @p@ via @pVia@.
--
-- Use this with visible type applications:
--
-- @
-- 'validate' = 'validateVia' \@pVia
-- @
--
-- Failures will record the name of @p@, followed by the @pVia@ failure
-- (together with its name). That is, we don't actually print the @'Via' pVia p@
-- name. (We're able to do this because filling out the
-- 'refineFailurePredicate' is left up to 'validate', instead of being handled
-- automatically. So think twice before changing that!)
validateVia
    :: forall pVia p a
    .  (Refine pVia a, Predicate p, KnownPredicateName p)
    => Proxy# p -> a -> Maybe RefineFailure
validateVia _p = validate (proxy# @(Via pVia p))
