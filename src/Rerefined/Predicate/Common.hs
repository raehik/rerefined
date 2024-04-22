{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Handy utilities for defining predicates.

module Rerefined.Predicate.Common
  (
  -- * Re-exports
    module Rerefined.Predicate
  , Typeably(..), Typeable
  , proxy#

  -- * Predicate validation
  , validateFail, validateBool

  -- * Predicate name
  , predicateName1, predicateName2
  ) where

import Rerefined.Predicate
import GHC.Exts ( Proxy#, proxy# )
import Data.Typeable.Typeably
import Data.Typeable ( Typeable )

-- | Shortcut for returning a predicate validation failure.
validateFail
    :: forall p
    .  Predicate p
    => Proxy# p -> String -> [RefineFailure String]
    -> Maybe (RefineFailure String)
validateFail p msg es = Just $ RefineFailure (predicateName p 0 "") msg es

-- | Shortcut for simply validating a 'Bool'.
validateBool
    :: Predicate p => Proxy# p -> String -> Bool
    -> Maybe (RefineFailure String)
validateBool p e = \case
  True  -> Nothing
  False -> validateFail p e []

predicateName1 :: forall p. Predicate p => String -> Int -> ShowS
predicateName1 pName d = showParen (d > 10) $
      showString pName . showChar ' '
    . predicateName (proxy# @p) 11

predicateName2
    :: forall l r. (Predicate l, Predicate r) => String -> Int -> ShowS
predicateName2 pName d = showParen (d > 10) $
      showString pName . showChar ' '
    . predicateName (proxy# @l) 11 . showChar ' '
    . predicateName (proxy# @r) 11
