module Rerefined.Predicate.Combinator.Xor where

import Rerefined.Predicate.Common

-- | Predicate exclusive disjunction.
data Xor l r

instance (Predicate l, Predicate r) => Predicate (Xor l r) where
    predicateName _ = predicateName2 @l @r "Xor"

instance (Refine l a, Refine r a) => Refine (Xor l r) a where
    validate p a =
        case validate (proxy# @l) a of
          Nothing ->
            case validate (proxy# @r) a of
              Nothing -> validateFail p "l&r succeeded" []
              Just _  -> Nothing
          Just el ->
            case validate (proxy# @r) a of
              Nothing -> Nothing
              Just er -> validateFail p "l&r failed" [el, er]
