-- | Predicate re-exports, for when you're heavily using refinement types.

module Rerefined.Predicates
  (
  -- * Base
    Succeed
  , Fail

  -- * Logical
  --, Not
  --, Logical

  -- * Relational
  , CompareValue
  , Sign(..)
  , RelOp(..)
  , CompareLength
  ) where

import Rerefined.Predicate.Succeed
import Rerefined.Predicate.Fail
--import Rerefined.Predicate.Logical
import Rerefined.Predicate.Relational
