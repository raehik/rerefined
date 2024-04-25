{-# LANGUAGE AllowAmbiguousTypes #-}

module Rerefined.Predicate.Logical where

import Rerefined.Predicate.Common
import Rerefined.Refined
import Rerefined.Refine.Unsafe

-- | Logical binary operator.
--
-- No need to disambiguate that these are binary operators, because there's only
-- one logical unary operator 'Not'.
data LogicOp = And | Or | Nand | Nor | Xor | Xnor

-- | A logical binary operation on two predicates.
data Logical (op :: LogicOp) l r

-- TODO could do whatever we want here e.g. infix. (but idk what e.g. XNOR uses)
instance (Predicate l, Predicate r, ReifyLogicOp op)
  => Predicate (Logical op l r) where
    predicateName _ d = showParen (d > 10) $
          showString "Logical "
        . showString (reifyLogicOpPretty @op) . showChar ' '
        . predicateName (proxy# @l) 11 . showChar ' '
        . predicateName (proxy# @r) 11

instance (Refine l a, Refine r a, ReifyLogicOp op)
  => Refine (Logical op l r) a where
    validate p a =
        reifyLogicOp @op (validateFail p)
            (validate (proxy# @l) a)
            (validate (proxy# @r) a)

-- | Reify a logical binary operator type tag.
class ReifyLogicOp (op :: LogicOp) where
    reifyLogicOpPretty :: String
    reifyLogicOp
        :: (String -> [a] -> Maybe a)
        -> Maybe a
        -> Maybe a
        -> Maybe a

instance ReifyLogicOp And where
    reifyLogicOpPretty = "And"
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
    reifyLogicOpPretty = "Or"
    reifyLogicOp fFail l r =
        case l of
          Nothing -> Nothing
          Just el ->
            case r of
              Nothing -> Nothing
              Just er -> fFail "OR:     l&r failed"    [el, er]

instance ReifyLogicOp Nand where
    reifyLogicOpPretty = "Nand"
    reifyLogicOp fFail l r =
        case l of
          Just _  -> Nothing
          Nothing ->
            case r of
              Just _  -> Nothing
              Nothing -> fFail "NAND:   l&r succeeded" [      ]

instance ReifyLogicOp Nor where
    reifyLogicOpPretty = "Nor"
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
    reifyLogicOpPretty = "Xor"
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
    reifyLogicOpPretty = "Xnor"
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
    predicateName _ = predicateName1 @p "Not"

instance Refine p a => Refine (Not p) a where
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
