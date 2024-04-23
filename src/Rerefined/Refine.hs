{-# LANGUAGE OverloadedStrings #-}

module Rerefined.Refine where

import Rerefined.Refined
import Rerefined.Refined1
import Rerefined.Predicate
import GHC.Exts ( proxy#, IsString )

refine
    :: forall p a. Refine p a
    => a -> Either (RefineFailure String) (Refined p a)
refine a =
    case validate (proxy# @p) a of
      Nothing -> Right (Refined a)
      Just e  -> Left e

refine1
    :: forall p f a. Refine1 p f
    => f a -> Either (RefineFailure String) (Refined1 p f a)
refine1 fa =
    case validate1 (proxy# @p) fa of
      Nothing -> Right (Refined1 fa)
      Just e  -> Left e

-- TODO needs work. boring & idk how to format nicely
prettyRefineFailure :: (Semigroup a, IsString a) => RefineFailure a -> a
prettyRefineFailure = go (0 :: Int) . (\e -> [e])
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
