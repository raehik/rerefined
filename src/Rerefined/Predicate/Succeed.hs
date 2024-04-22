module Rerefined.Predicate.Succeed where

import Rerefined.Predicate.Common

-- | The unit predicate. Always succeeds.
data Succeed deriving Predicate via Typeably Succeed

instance Refine Succeed a where
    validate _ _ = Nothing
