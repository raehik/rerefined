module Rerefined.Predicate.Combinator.And where

import Rerefined.Predicate.Common
import Rerefined.Refined

-- | Predicate conjunction.
data And l r

instance (Predicate l, Predicate r) => Predicate (And l r) where
    predicateName _ = predicateName2 @l @r "And"

instance (Refine l a, Refine r a) => Refine (And l r) a where
    validate p a =
        case validate (proxy# @l) a of
          Nothing ->
            case validate (proxy# @r) a of
              Nothing -> Nothing
              Just er -> validateFail p "right failed" [er]
          Just el ->
            case validate (proxy# @r) a of
              Nothing -> validateFail p  "left failed" [el]
              Just er -> validateFail p   "l&r failed" [el, er]

-- TODO names

reassocRL :: Refined (And l r) a -> Refined r (Refined l a)
reassocRL = Refined . Refined . unrefine

reassocLR :: Refined (And l r) a -> Refined l (Refined r a)
reassocLR = Refined . Refined . unrefine
