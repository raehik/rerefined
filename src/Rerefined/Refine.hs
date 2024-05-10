{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- for predicate reification

module Rerefined.Refine
  (
  -- * @Refined@
    type Refined
  , refine
  , unrefine
  , reifyPredicate

  -- * @Refined1@
  , type Refined1
  , refine1
  , unrefine1
  , reifyPredicate1

  -- * Errors
  , type RefineFailure
  , prettyRefineFailure
  , prettyRefineFailure'
  ) where

import Rerefined.Refined
import Rerefined.Predicate
import GHC.Exts ( proxy# )
import Data.Text ( Text )
import Data.Text.Builder.Linear qualified as TBL

-- | Refine @a@ with predicate @p@.
refine
    :: forall p a. Refine p a
    => a -> Either RefineFailure (Refined p a)
refine a =
    case validate (proxy# @p) a of
      Nothing -> Right (Refined a)
      Just e  -> Left e

-- | Reify a predicate.
reifyPredicate :: forall p a. Refine p a => a -> Bool
reifyPredicate a = case refine @p a of Right{} -> True; Left{} -> False

-- | Refine @f a@ with functor predicate @p@.
refine1
    :: forall p f a. Refine1 p f
    => f a -> Either RefineFailure (Refined1 p f a)
refine1 fa =
    case validate1 (proxy# @p) fa of
      Nothing -> Right (Refined1 fa)
      Just e  -> Left e

-- | Reify a functor predicate.
reifyPredicate1 :: forall p f a. Refine1 p f => f a -> Bool
reifyPredicate1 fa = case refine1 @p fa of Right{} -> True; Left{} -> False

{- TODO
* got an extra \n at start oops
* make it look better lol
* make tail-call recursive? need to ferry more indents around though
-}
prettyRefineFailure :: RefineFailure -> Text
prettyRefineFailure = TBL.runBuilder . go (0 :: Int) . (\e -> [e])
  where
    go n = \case
      []     -> mempty
      (e:es) ->
        let bPred   = TBL.fromChar '\n' <> indent n <> refineFailurePredicate e
            bDetail = TBL.fromChar '\n' <> indent (n+2) <> refineFailureDetail e
         in bPred <> bDetail <> go (n+2) (refineFailureInner e) <> go n es
    indent = \case
      0 -> mempty
      n -> TBL.fromChar ' ' <> indent (n-1)

-- TODO this requires switching inner errors so that last is first lol x)
-- also we only <> right. maybe that's useful for perf
-- to remove newline at start we need to start in a special mode so we know not
-- to add the first newline at bPred. but I cba it will be messier and I want to
-- replace this ASAP when someone comes up with a good pretty error format
prettyRefineFailure' :: RefineFailure -> Text
prettyRefineFailure' = \e -> TBL.runBuilder $ go mempty [(0 :: Int, e)]
  where
    go b = \case
      []            -> b
      ((n, e) : es) ->
        let bPred   = TBL.fromChar '\n' <> indent n <> refineFailurePredicate e
            bDetail = TBL.fromChar '\n' <> indent (n+2) <> refineFailureDetail e
            b'      = b <> bPred <> bDetail
         in go b' (idk (n+2) es (refineFailureInner e))
    indent = \case
      0 -> mempty
      n -> TBL.fromChar ' ' <> indent (n-1)
    idk n rs = \case
      []   -> rs
      l:ls -> idk n ((n, l):rs) ls
