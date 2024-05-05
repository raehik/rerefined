{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Base definitions for refinement predicates.

module Rerefined.Predicate
  ( Refine(validate)
  , Refine1(validate1)
  , RefineFailure(..)
  , Predicate(..)
  , predicateName
  ) where

import GHC.Exts ( Proxy#, proxy# )
import Data.Text.Builder.Linear qualified as TBL
import GHC.TypeLits ( Natural, Symbol, KnownSymbol, symbolVal' )

-- | Types which define refinements on other types.
class Predicate p where
    -- | Predicate name.
    --
    -- Predicate names should aim to communicate the meaning of the predicate as
    -- clearly and concisely as possible.
    --
    -- Consider using @type-level-show@ to build this. However, note that GHC
    -- cannot figure out 'KnownSymbol' when there are type families in play, so
    -- you may need to put 'KnownSymbol' constraints in instance contexts.
    type PredicateName (d :: Natural) p :: Symbol
        -- ^ TODO d: the operator precedence of the enclosing context (a number
        --   from 0 to 11). Function application has precedence 10.

{- TODO
stuffing the KnownSymbol constraint into a Predicate superclass is handy, but
then we have to handle it in combinator predicates. probably _not_ doing so is
better, so I'm trying that first.
-}

-- | Reify predicate name.
predicateName
    :: forall p. (Predicate p, KnownSymbol (PredicateName 0 p)) => String
predicateName = symbolVal' (proxy# @(PredicateName 0 p))

-- | Refine @a@ with predicate @p@.
class Predicate p => Refine p a where
    -- | Validate predicate @p@ for the given @a@.
    --
    -- 'Nothing' indicates success. 'Just' contains a validation failure.
    validate :: Proxy# p -> a -> Maybe RefineFailure

-- | Refine functor type @f@ with functor predicate @p@.
--
-- By not making the contained type accessible, we ensure refinements apply
-- @forall a. f a@. That is, refinements here apply only to the functor
-- structure, and not the stored elements.
class Predicate p => Refine1 p f where
    -- | Validate predicate @p@ for the given @f a@.
    validate1 :: Proxy# p -> f a -> Maybe RefineFailure

-- | Predicate validation failure.
data RefineFailure = RefineFailure
  { refineFailurePredicate :: TBL.Builder
  -- ^ The name of the predicate that failed.
  --
  -- Obtained via 'predicateName'.

  , refineFailureDetail    :: TBL.Builder
  -- ^ Precise failure detail e.g. which of the inner predicates of an @And@
  --   combinator predicate failed.

  , refineFailureInner     :: [RefineFailure]
  -- ^ Any wrapped errors, for combinator predicates.
  --
  -- What these are, and their order, should be noted in 'refineFailureDetail'.
  } deriving stock Show
