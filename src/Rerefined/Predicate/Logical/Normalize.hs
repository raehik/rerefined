module Rerefined.Predicate.Logical.Normalize where

import Rerefined.Predicate.Logical

type family NormLogi p where
    NormLogi (Not (Not p)) = Just p
    NormLogi (Or   l l)    = Just l
    NormLogi (And  l l)    = Just l
    NormLogi (Nand l l)    = Just (Not l)
    NormLogi p             = Nothing
