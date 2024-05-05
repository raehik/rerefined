# rerefined to-dos
* think about reassociation, other weird utils (refined actually provides lots)
* think about strengthening and weakening. do want but the naming overlaps with
  my strongweak library... maybe that's just life? not a massive issue
* check the refined issue list, see if I have any outstanding
  * on a glance no: added more reassociativity helpers, no aeson, removed
    insidious Typeable constraints, added `Refined1`, (strongweak), removed
    `These`
* make a little `prettyRefined :: Show a => Refined p a -> String` that reifies
  the predicate nicely and slots the value in!
  * hrmmmmm idk exactly how it'll look. maybe leave till later.

## Errors: merge detail and inner?
With my predicate name improvements, the refinement failure detail field hardly
seems to state anything anymore. Perhaps all it need do is disambiguate what
inner failures are. Like:

```haskell
data E a = E { eName :: a, eInner :: [(a, E a)] }
```

Perhaps the main question here is, what should the pretty error look like.

## Strengthening & weakening
* the `Weaken` type class should probably be implemented another way
* start with relational ops

## Instances
### Avoid
* I think the aeson instances for `Refined` aren't good... I might have a
  predicate that wants to alter JSON schema but now I have to re-newtype?

## `Text` in failures
`ShowS` is acceptable for short inputs-- but it's embedded in a larger string,
and that string eventually must be printed. We are much better off using `Text`.

The lovely #haskell folks pointed me to `text-builder-linear`. If I could write
a `TypeRep` pretty printer in that, I may be able to use it. Otherwise, I can
stick with `text-show` or `text-display`, which both use the lazy `Text`
builder.

## Terminology
* read papers
  * https://arxiv.org/pdf/2010.07763.pdf
    * arxiv can break idk. https://arxiv.org/abs/2010.07763
  * https://prl.khoury.northeastern.edu/blog/static/refinement_types_lecture.pdf
  * https://www.google.com/search?q=refinement+types+paper
