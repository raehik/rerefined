{-# LANGUAGE UndecidableInstances #-} -- for easier PredicateName
{-# LANGUAGE OverloadedStrings #-} -- for builder

module Rerefined.Predicate.Logical.And where

import Rerefined.Predicate.Common.Binary
import Rerefined.Predicate.Common

-- | Logical conjunction. Also AND logic gate.
data And l r

-- | Precendence of 3 (matching 'Data.Bool.&&').
instance (Predicate l, Predicate r) => Predicate (And l r) where
    type PredicateName d (And l r) = PredicateNameBOp " ∧ " 3 d l r

instance (Refine l a, Refine r a, KnownPredicateName (And l r))
  => Refine (And l r) a where
    validate p a =
        case l of
          Nothing ->
            case r of
              Nothing -> Nothing
              Just er -> validateFail p "AND:  right failed"    [er    ]
          Just el ->
            case r of
              Nothing -> validateFail p "AND:   left failed"    [    el]
              Just er -> validateFail p "AND:    l&r failed"    [er, el]
      where
        l = validate (proxy# @l) a
        r = validate (proxy# @r) a
