module Rerefined.Refined where

import Language.Haskell.TH.Syntax ( Lift )

-- | @a@ refined with predicate @p@.
newtype Refined p a = Refined { unrefine :: a }
    deriving stock (Lift, Show) -- TODO Show? useful but meh?
