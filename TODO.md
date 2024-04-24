# rerefined to-dos
* turn unrefine into a regular function instead of a record. makes stock Show
  instance nicer lol. (document this decision too)
* clarify unsafe refines etc.
* think about reassociation, other weird utils (refined actually provides lots)
* think about strengthening and weakening. do want but the naming overlaps with
  my strongweak library... maybe that's just life? not a massive issue
* check the refined issue list, see if I have any outstanding
  * on a glance no: added more reassociativity helpers, no aeson, removed
    insidious Typeable constraints, added `Refined1`, (strongweak), removed
    `These`

## Instances
### Avoid
* I think the aeson instances for `Refined` aren't good... I might have a
  predicate that wants to alter JSON schema but now I have to re-newtype?
