module Rerefined.Refined1 where

-- | @f a@ refined with predicate @p@.
newtype Refined1 p f a = Refined1 { unrefine1 :: f a }
    deriving stock Show
