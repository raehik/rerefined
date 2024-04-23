# rerefined to-dos
* clarify unsafe refines etc.
* think about reassociation, other weird utils (refined actually provides lots)
* think about strengthening and weakening. do want but the naming overlaps with
  my strongweak library... maybe that's just life? not a massive issue
* check the refined issue list, see if I have any outstanding
  * on a glance no: added more reassociativity helpers, no aeson, removed
    insidious Typeable constraints, added `Refined1`, (strongweak), removed
    `These`

## Instances
* vector...? but I think it's handled by `Foldable`
* MonoTraversable for many `Refined1->Refined` predicates! for that reason, I
  should avoid even `Refine (CompareLength n) Text`!!
  * oh wow mono-traversable is fairly small! good god it only has a handful of
    non-base dependencies. seems good for core library inclusion honestly
* I think the aeson instances for `Refined` aren't good... I might have a
  predicate that wants to alter JSON schema but now I have to re-newtype?
