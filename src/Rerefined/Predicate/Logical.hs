{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- for PredicateName

module Rerefined.Predicate.Logical where

import Rerefined.Predicate.Common
import Rerefined.Refined
import Rerefined.Refine.Unsafe
import TypeLevelShow.Utils
import GHC.TypeLits ( Symbol )

-- | Logical binary operator.
--
-- No need to disambiguate that these are binary operators, because there's only
-- one logical unary operator 'Not'.
data LogicOp = And | Or | Nand | Nor | Xor | Xnor

type family ShowLogicOp (op :: LogicOp) :: Symbol where
    ShowLogicOp And  = "&&"
    ShowLogicOp Or   = "||"
    ShowLogicOp Nand = "!&"
    ShowLogicOp Nor  = "!|"
    ShowLogicOp Xor  = "^^"
    ShowLogicOp Xnor = "!^"

-- | A logical binary operation on two predicates.
data Logical (op :: LogicOp) l r

-- | TODO making all ops pred 3. but || is infixr 2 in base. meh it's up to me,
--   not really sure.
instance (Predicate l, Predicate r) => Predicate (Logical op l r) where
    type PredicateName d (Logical op l r) = ShowParen (d > 3)
        (    PredicateName 4 l ++ ShowChar ' '
          ++ ShowLogicOp op    ++ ShowChar ' '
          ++ PredicateName 4 r )

instance
  ( Refine l a, Refine r a, ReifyLogicOp op
  , KnownPredicateName (Logical op l r)
  ) => Refine (Logical op l r) a where
    validate p a =
        reifyLogicOp @op (validateFail p)
            (validate (proxy# @l) a)
            (validate (proxy# @r) a)

-- | Reify a logical binary operator type tag.
class ReifyLogicOp (op :: LogicOp) where
    reifyLogicOp
        :: (Builder -> [a] -> Maybe a)
        -> Maybe a
        -> Maybe a
        -> Maybe a

instance ReifyLogicOp And where
    reifyLogicOp fFail l r =
        case l of
          Nothing ->
            case r of
              Nothing -> Nothing
              Just er -> fFail "AND:  right failed"    [    er]
          Just el ->
            case r of
              Nothing -> fFail "AND:   left failed"    [el    ]
              Just er -> fFail "AND:    l&r failed"    [el, er]

instance ReifyLogicOp Or where
    reifyLogicOp fFail l r =
        case l of
          Nothing -> Nothing
          Just el ->
            case r of
              Nothing -> Nothing
              Just er -> fFail "OR:     l&r failed"    [el, er]

instance ReifyLogicOp Nand where
    reifyLogicOp fFail l r =
        case l of
          Just _  -> Nothing
          Nothing ->
            case r of
              Just _  -> Nothing
              Nothing -> fFail "NAND:   l&r succeeded" [      ]

instance ReifyLogicOp Nor where
    reifyLogicOp fFail l r =
        case l of
          Just el ->
            case r of
              Just _  -> Nothing
              Nothing -> fFail "NOR:  right succeeded" [el    ]
          Nothing ->
            case r of
              Just er -> fFail "NOR:   left succeeded" [    er]
              Nothing -> fFail "NOR:    l&r succeeded" [      ]

instance ReifyLogicOp Xor where
    reifyLogicOp fFail l r =
        case l of
          Nothing ->
            case r of
              Just _  -> Nothing
              Nothing -> fFail "XOR:    l&r succeeded" [      ]
          Just el ->
            case r of
              Nothing -> Nothing
              Just er -> fFail "XOR:    l&r failed"    [el, er]

instance ReifyLogicOp Xnor where
    reifyLogicOp fFail l r =
        case l of
          Nothing ->
            case r of
              Nothing -> Nothing
              Just er -> fFail "XNOR: right failed"    [    er]
          Just el ->
            case r of
              Just _  -> Nothing
              Nothing -> fFail "XNOR:  left failed"    [el    ]

data Not p

instance Predicate p => Predicate (Not p) where
    -- TODO not sure on precedence here
    type PredicateName d (Not p) = ShowParen (d > 10) "¬ " ++ PredicateName 11 p

instance (Refine p a, KnownPredicateName (Not p))
  => Refine (Not p) a where
    validate p a =
        case validate (proxy# @p) a of
          Just _  -> Nothing
          Nothing -> validateFail p "NOT: predicate succeeded" []

-- TODO principle of explosion? (p and not p -> anything)

-- TODO
rerefineDeMorgans1
    :: Refined (Not (Logical Or  l r))       a
    -> Refined (Logical And (Not l) (Not r)) a
rerefineDeMorgans1 = unsafeRerefine

-- TODO
rerefineDeMorgans2
    :: Refined (Not (Logical And l r))       a
    -> Refined (Logical Or  (Not l) (Not r)) a
rerefineDeMorgans2 = unsafeRerefine
