{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Handy utilities for defining predicates.

module Rerefined.Predicate.Common
  (
  -- * Re-exports
    module Rerefined.Predicate
  , Typeably(..), Typeable
  , proxy#
  , TBL.Builder
  , tshowParen
  , tshowChar
  , IsString

  -- * Predicate validation
  , validateFail, validateBool

  -- * Predicate name
  , predicateName1, predicateName2
  ) where

import Rerefined.Predicate
import GHC.Exts ( Proxy#, proxy#, IsString )
import Data.Typeable.Typeably
import Data.Typeable ( Typeable )
import Data.Text.Builder.Linear qualified as TBL
import Data.Text ( Text )
import Raehik.Data.Text.Builder.Linear.ShowUtils ( tshowParen )

-- | Shortcut for returning a predicate validation failure.
validateFail
    :: forall p
    .  Predicate p
    => Proxy# p -> TBL.Builder -> [RefineFailure]
    -> Maybe RefineFailure
validateFail p msg es = Just $ RefineFailure (predicateName p 0) msg es

-- | Shortcut for simply validating a 'Bool'.
validateBool
    :: Predicate p => Proxy# p -> TBL.Builder -> Bool
    -> Maybe RefineFailure
validateBool p e = \case
  True  -> Nothing
  False -> validateFail p e []

predicateName1 :: forall p. Predicate p => Text -> Int -> TBL.Builder
predicateName1 pName d = tshowParen (d > 10) $
       TBL.fromText pName <> TBL.fromChar ' '
    <> predicateName (proxy# @p) 11

predicateName2
    :: forall l r. (Predicate l, Predicate r) => Text -> Int -> TBL.Builder
predicateName2 pName d = tshowParen (d > 10) $
       TBL.fromText pName <> TBL.fromChar ' '
    <> predicateName (proxy# @l) 11 <> TBL.fromChar ' '
    <> predicateName (proxy# @r) 11

-- | Renamed 'TBL.fromChar'.
tshowChar :: Char -> TBL.Builder
tshowChar = TBL.fromChar
