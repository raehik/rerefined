module Rerefined.Refined1 where

import Language.Haskell.TH.Syntax ( Lift )

-- | @f a@ refined with predicate @p@.
newtype Refined1 p f a = Refined1 { unrefine1 :: f a }
    deriving stock (Lift, Show) -- TODO Show? useful but meh?
