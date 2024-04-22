* add Refined1
* add missing combinator helpers (`And` reassociating)
* improve error handling (it's so weird)
* remove huge dependencies (go away aeson)
* fix other outstanding issues

---

OK, how to fix original lib:

  * `class Pred (a :: k) where predName' :: Text`
    * `Text`-ed `TypeRep`. Provide default.
    * Do manually for logical predicates (which take other predicates) to avoid
      `Typeable` contexts. Means unideal bracketing (unless we put more thought
      into it)... but does allow us to omit the implicit kind variables which
      show up e.g. `And * * l r`.
  * `throwRefineOtherException` should insert `predName` for us.

Every predicate comes with a simple "before" explanation (its name, static), and
a more detailed "after" explanation (a specific usage, runtime). This *is
already there.* It's just messy due to `Typeable`s.
