-- | Predicate re-exports, for when you're heavily using refinement types.

module Rerefined.Predicates
  (
  -- * Base
    Succeed
  , Fail

  -- * Logical
  , And, Iff, If, Nand, Nor, Not, Or, Xor

  -- * Relational
  , CompareValue
  , Sign(..)
  , CompareLength
  , LTE, GTE
  ) where

import Rerefined.Predicate.Succeed
import Rerefined.Predicate.Fail
import Rerefined.Predicate.Logical
import Rerefined.Predicate.Relational
