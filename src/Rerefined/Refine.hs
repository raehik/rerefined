{-# LANGUAGE OverloadedStrings #-}

module Rerefined.Refine where

import Rerefined.Refined
import Rerefined.Predicate
import GHC.Exts ( proxy#, IsString )

refine
    :: forall p a. Refine p a
    => a -> Either (RefineFailure String) (Refined p a)
refine a =
    case validate (proxy# @p) a of
      Nothing -> Right (Refined a)
      Just e  -> Left e

-- TODO needs work. boring & idk how to format nicely
refineFailurePretty :: (Semigroup a, IsString a) => RefineFailure a -> a
refineFailurePretty = go (0 :: Int) . (\e -> [e])
  where
    go n = \case
      []     -> ""
      (e:es) ->
           "\n" <> indent n     <> refineFailurePredicate e
        <> "\n" <> indent (n+2) <> refineFailureDetail    e
        <> go (n+2) (refineFailureInner e)
        <> go n es
    indent = \case
      0 -> ""
      n -> " " <> indent (n-1)
