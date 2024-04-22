module Rerefined.Predicate.Combinator.Or where

import Rerefined.Predicate.Common

-- | Predicate disjunction.
data Or l r

instance (Predicate l, Predicate r) => Predicate (Or l r) where
    predicateName _ = predicateName2 @l @r "Or"

instance (Refine l a, Refine r a) => Refine (Or l r) a where
    validate p a =
        case validate (proxy# @l) a of
          Nothing -> Nothing
          Just el ->
            case validate (proxy# @r) a of
              Nothing -> Nothing
              Just er -> validateFail p "l&r failed" [el, er]
