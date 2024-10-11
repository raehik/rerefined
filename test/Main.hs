module Main where

import Test.TypeSpec hiding ( And )
import Rerefined.Simplify
import Rerefined.Predicates
import Rerefined.Predicate
import TypeLevelShow.Utils ( type (++) )

main :: IO ()
main = print spec

spec :: Expect
    (And Fail    Succeed `SimplifiesTo` Fail
 -/- And Succeed Fail    `SimplifiesTo` Fail
 -/- And Succeed Succeed `SimplifiesTo` Succeed
 -/- And Fail    Fail    `SimplifiesTo` Fail

 -/- Or  Succeed Fail    `SimplifiesTo` Succeed
 -/- Or  Fail    Succeed `SimplifiesTo` Succeed
 -/- Or  Succeed Succeed `SimplifiesTo` Succeed
 -/- Or  Fail    Fail    `SimplifiesTo` Fail
 -/- And (CompareLength RelOpLT 3) (CompareLength RelOpGT 3)
        `SimplifiesTo` Fail
    )
spec = Valid

-- | TODO I'd like to write my own message here, but type-spec is complicated.
type SimplifiesTo p p' = It
    (PredicateName 0 p ++ " simplifies to " ++ PredicateName 0 p')
    (TrySimplify p `Is` Just p')
