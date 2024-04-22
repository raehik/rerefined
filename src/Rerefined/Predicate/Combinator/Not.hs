module Rerefined.Predicate.Combinator.Not where

import Rerefined.Predicate.Common

data Not p

instance Predicate p => Predicate (Not p) where
    predicateName _ = predicateName1 @p "Not"

instance Refine p a => Refine (Not p) a where
    validate p a =
        case validate (proxy# @p) a of
          Just _  -> Nothing
          Nothing -> validateFail p "predicate succeeded" []

-- TODO principle of explosion? (p and not p -> anything)
