{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Handy utilities for defining predicates.

module Rerefined.Predicate.Common
  (
  -- * Re-exports
    module Rerefined.Predicate
  , Typeably(..), Typeable
  , proxy#
  , TBL.Builder
  , IsString -- TODO remove

  -- * Predicate validation
  , validateFail, validateBool
  , KnownPredicateName
  ) where

import Rerefined.Predicate
import GHC.Exts ( Proxy#, proxy#, IsString(fromString) )
import GHC.TypeLits ( KnownSymbol, Symbol )
import Data.Typeable.Typeably
import Data.Typeable ( Typeable )
import Data.Text.Builder.Linear qualified as TBL
import Data.Text ( Text )

type KnownPredicateName p = KnownSymbol (PredicateName 0 p)

-- | Shortcut for returning a predicate validation failure.
validateFail
    :: forall p
    .  (Predicate p, KnownPredicateName p)
    => Proxy# p -> TBL.Builder -> [RefineFailure]
    -> Maybe RefineFailure
validateFail p msg es = Just $ RefineFailure (fromString $ predicateName @p) msg es

-- | Shortcut for simply validating a 'Bool'.
validateBool
    :: forall p
    .  (Predicate p, KnownPredicateName p)
    => Proxy# p -> TBL.Builder -> Bool
    -> Maybe RefineFailure
validateBool p e = \case
  True  -> Nothing
  False -> validateFail p e []
