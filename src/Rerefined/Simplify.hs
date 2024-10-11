{-# LANGUAGE UndecidableInstances #-}

{- | Primitive predicate simplifier.

This is related to an NP-complete problem (see Boolean satisfiability problem).
We focus on /immediate, operation-reducing simplifications/, and hope that the
input is formed in such a way that our rules match.

The simplifier may not be expectected to consistently implement any
transformations whatsoever. The only guarantees are

* the output has the same or fewer operations
* the output meaning is identical to the input

Implementation pitfalls:

* not extensible: only works for built-in logical & relational predicates
* no protection against non-termination e.g. if a pair of transformations loop
* very tedious to write. that's life
-}

module Rerefined.Simplify
  ( Simplify
  , SimplifyStep
  ) where

import Rerefined.Predicate.Succeed
import Rerefined.Predicate.Fail

import Rerefined.Predicate.Logical

import Rerefined.Predicate.Relational ( CompareLength, FlipRelOp )
import Rerefined.Simplify.Relational
  ( SimplifyCompareLength
  , SimplifyCompareLengthAnd
  , SimplifyCompareLengthOr
  )

import Data.Kind ( Type )

-- note that we can't modularize logical simplifications because they're
-- mutually recursive with the main simplifier

-- | Simplify the given predicate.
--
-- Returns the input predicate if we were unable to simplify.
type Simplify :: Type -> Type
type Simplify p = Simplify' p

-- | Helper definition for reducing duplication.
type Simplify' p = SimplifyLoop p (SimplifyStep p)

-- | Simplification loop.
type family SimplifyLoop p mp where
    -- simplification step succeeded: try again on the simplified predicate
    SimplifyLoop p (Just p') = Simplify' p'

    -- failed to simplify: give up, return the latest predicate
    SimplifyLoop p Nothing   = p

-- | Try to perform a single simplification step on the given predicate.
--
-- Returns 'Nothing' if we were unable to simplify.
type family SimplifyStep p where
    SimplifyStep (And  l r) = SimplifyAnd  l r
    SimplifyStep (Or   l r) = SimplifyOr   l r
    SimplifyStep (Nand l r) = SimplifyNand l r
    SimplifyStep (Not  p)   = SimplifyNot  p

    SimplifyStep (CompareLength op n) = SimplifyCompareLength op n

    SimplifyStep p = Nothing

type family SimplifyAnd l r where
    -- identity laws
    SimplifyAnd p       Fail    = Just Fail
    SimplifyAnd Fail    p       = Just Fail
    SimplifyAnd p       Succeed = Just p
    SimplifyAnd Succeed p       = Just p

    SimplifyAnd p p = Just p

    -- distributivity
    SimplifyAnd (Or x y) (Or x z) = Just (Or x (And y z))

    -- special
    SimplifyAnd (CompareLength lop ln) (CompareLength rop rn) =
        SimplifyCompareLengthAnd lop ln rop rn

    -- recurse
    SimplifyAnd l r =
        (OrElseAndL r (SimplifyStep l)
            (OrElseAndR l (SimplifyStep r)
                Nothing))

type family OrElseAndL r mp cont where
    OrElseAndL r Nothing   cont = cont
    OrElseAndL r (Just l') cont = Just (And l' r)

type family OrElseAndR l mp cont where
    OrElseAndR l Nothing   cont = cont
    OrElseAndR l (Just r') cont = Just (And l r')

type family SimplifyOr l r where
    -- identity laws
    SimplifyOr Succeed p       = Just Succeed
    SimplifyOr p       Succeed = Just Succeed
    SimplifyOr Fail    p       = Just p
    SimplifyOr p       Fail    = Just p

    SimplifyOr p p = Just p

    -- distributivity
    SimplifyOr (And x y) (And x z) = Just (And x (Or y z))

    -- special
    SimplifyOr (CompareLength lop ln) (CompareLength rop rn) =
        SimplifyCompareLengthOr lop ln rop rn

    -- recurse
    SimplifyOr l r =
        (OrElseOrL r (SimplifyStep l)
            (OrElseOrR l (SimplifyStep r)
                Nothing))

type family OrElseOrL r mp cont where
    OrElseOrL r Nothing   cont = cont
    OrElseOrL r (Just l') cont = Just (Or l' r)

type family OrElseOrR l mp cont where
    OrElseOrR l Nothing   cont = cont
    OrElseOrR l (Just r') cont = Just (Or l r')

type family SimplifyNand l r where
    -- TODO fill in rest
    SimplifyNand p p = Just (Not p)

    -- recurse
    SimplifyNand l r =
        (OrElseNandL r (SimplifyStep l)
            (OrElseNandR l (SimplifyStep r)
                Nothing))

type family OrElseNandL r mp cont where
    OrElseNandL r Nothing   cont = cont
    OrElseNandL r (Just l') cont = Just (Nand l' r)

type family OrElseNandR l mp cont where
    OrElseNandR l Nothing   cont = cont
    OrElseNandR l (Just r') cont = Just (Nand l r')

type family SimplifyNot p where
    -- double negation
    SimplifyNot (Not p) = Just p

    SimplifyNot Succeed = Just Fail
    SimplifyNot Fail    = Just Succeed

    SimplifyNot (CompareLength op n) = Just (CompareLength (FlipRelOp op) n)

    -- recurse
    SimplifyNot p = OrElseNot (SimplifyStep p) Nothing

type family OrElseNot mp cont where
    OrElseNot Nothing   cont = cont
    OrElseNot (Just p') cont = Just (Not p')
