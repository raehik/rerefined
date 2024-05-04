{-# LANGUAGE OverloadedStrings #-}

module Rerefined.Predicate.Fail where

import Rerefined.Predicate.Common

-- | Always fails.
data Fail deriving Predicate via Typeably Fail

instance Refine Fail a where
    validate p _ = validateFail p "fail" []
