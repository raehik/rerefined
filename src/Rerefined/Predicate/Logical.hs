{-# LANGUAGE UndecidableInstances #-} -- TODO TMP

module Rerefined.Predicate.Logical
  ( And, Iff, If, Nand, Nor, Not, Or, Xor
  , NormalizeLogicalStep
  , NormalizeLogical1Step
  , Result(..), Idk1'
  ) where

import Rerefined.Predicate.Logical.And
import Rerefined.Predicate.Logical.Iff
import Rerefined.Predicate.Logical.If
import Rerefined.Predicate.Logical.Nand
import Rerefined.Predicate.Logical.Nor
import Rerefined.Predicate.Logical.Not
import Rerefined.Predicate.Logical.Or
import Rerefined.Predicate.Logical.Xor

import Data.Kind ( Type )
-- left = continue, right = normalized
type NormalizeLogicalStep :: (kl -> kr -> Type) -> kl -> kr -> Result kl
type family NormalizeLogicalStep op l r where
    NormalizeLogicalStep Or   l l = Cont  l
    NormalizeLogicalStep And  l l = Cont  l
    NormalizeLogicalStep Nand l l = Cont1 (Not l)
    NormalizeLogicalStep op   l r = Done (op l r)

data Result kl = Done Type | Cont kl | Cont1 Type

type NormalizeLogical1Step :: (k -> Type) -> k -> Result k
type family NormalizeLogical1Step op p where
    NormalizeLogical1Step Not (Not p) = Cont p
    NormalizeLogical1Step op  p       = Done (op p)

type family Idk1 res where
    Idk1 (Cont p) = Idk1' p
    Idk1 (Done p) = p

type family Idk1' p where
    Idk1' (Not p) = Idk1 (NormalizeLogical1Step Not p)
    Idk1' p = p
