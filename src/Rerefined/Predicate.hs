-- {-# LANGUAGE AllowAmbiguousTypes #-}

-- | Base definitions for refinement predicates.

module Rerefined.Predicate
  ( Refine(validate)
  , Refine1(validate1)
  , RefineFailure(..)
  , Predicate(predicateName)
  ) where

import GHC.Exts ( Proxy# )
import Data.Typeable ( Typeable, typeRep )
import Data.Typeable.Typeably
import Data.Proxy ( Proxy(Proxy) )
import Data.Text.Builder.Linear qualified as TBL
import GHC.Exts ( fromString )

-- | Types which define refinements on other types.
class Predicate p where
    -- | Predicate name.
    --
    -- Predicate names should aim to communicate the meaning of the predicate as
    -- clearly and concisely as possible.
    --
    -- For non-combinator predicates (i.e. your predicate does not wrap another
    -- predicate), this may be derived this via the type's 'TypeRep'. Derive via
    -- @'Typeably' a@.
    --
    -- Combinator predicates must define this manually, as using 'Typeable'
    -- would incur insidious 'Typeable' contexts for wrapped predicates.
    -- TODO should be some generics and/or TH I can write to resolve this.
    --
    -- It is suggested that you stick with the 'TypeRep' of your type, as this
    -- is usually clear and always easiest to map back to definitions. However,
    -- you may wish to override this e.g. if your type is highly parameterized
    -- and you wish to hide this for simplicity (see
    -- 'Rerefined.Predicate.Logical.Logical').
    predicateName
        :: Proxy# p
        -> Int
        -- ^ the operator precedence of the enclosing context (a number from 0
        --   to 11). Function application has precedence 10.
        -> TBL.Builder

-- | Fill out predicate name using the type's 'TypeRep', via 'Typeable'.
--
-- Do not use this for combinator predicates. Doing so will incur insidious
-- 'Typeable' contexts for wrapped predicates.
instance Typeable a => Predicate (Typeably a) where
    -- TODO use my new text one guh...
    predicateName _ d = fromString $ showsPrec d (typeRep (Proxy @a)) ""

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

--prettyPredicate :: forall p. Predicate p => String
