-- | Base definitions for refinement predicates.

module Rerefined.Predicate
  ( Predicate(predicateName)
  , Refine(validate)
  , Refine1(validate1)
  , RefineFailure(..)
  ) where

import GHC.Exts ( Proxy# )
import Data.Typeable ( Typeable, typeRep )
import Data.Typeable.Typeably
import Data.Proxy ( Proxy(Proxy) )

-- | Types which define refinements on other types.
class Predicate p where
    -- | The predicate name, as a 'Show'-like.
    --
    -- TODO. It should be like 'Show'. You can use 'typeRep' to fill it out for
    -- non-combinator predicates. Combinator predicates need to write an
    -- effective 'Show' instance.
    predicateName :: Proxy# p -> Int -> ShowS

-- | Fill out predicate metadata using its 'Typeable' instance.
--
-- Do not use this for combinator predicates. Doing so will incur
-- insidious 'Typeable' contexts for the wrapped predicate(s).
instance Typeable a => Predicate (Typeably a) where
    predicateName _ d = showsPrec d (typeRep (Proxy @a))

-- | Refine @a@ with predicate @p@.
class Predicate p => Refine p a where
    -- | Validate predicate @p@ for the given @a@.
    --
    -- 'Nothing' indicates success. 'Just' contains a validation failure.
    validate :: Proxy# p -> a -> Maybe (RefineFailure String)

-- | Refine functor type @f@ with functor predicate @p@.
--
-- By not making the contained type accessible, we ensure refinements apply
-- @forall a. f a@.
class Predicate p => Refine1 p f where
    -- | Validate predicate @p@ for the given @f a@.
    validate1 :: Proxy# p -> f a -> Maybe (RefineFailure String)

-- | Predicate validation failure.
--
-- Polymorphic over the message type because I want to use 'Text', but want it
-- doesn't have the convenient 'Show' internals that 'String' does.
data RefineFailure a = RefineFailure
  { refineFailurePredicate :: a
  -- ^ The predicate that failed.

  , refineFailureDetail    :: a
  -- ^ Failure clarification.

  , refineFailureInner     :: [RefineFailure a]
  -- ^ Any wrapped errors, for combinator predicates.
  --
  -- What these are, and their order, should be noted in 'refineFailureDetail'.
  }
