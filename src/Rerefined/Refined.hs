module Rerefined.Refined where

-- | @a@ refined with predicate @p@.
newtype Refined p a = Refined { unrefine :: a }
    deriving stock Show
