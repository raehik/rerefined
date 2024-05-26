{-# LANGUAGE UndecidableInstances #-}

module Rerefined.Predicate.Normalize where

import Rerefined.Predicate.Logical.Normalize

type family Norm p where
    Norm p = Norm' p (NormLogi p)

type family Norm' p mp where
    Norm' p Nothing   = p
    Norm' p (Just p') = Norm p'
