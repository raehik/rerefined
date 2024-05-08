# rerefined to-dos
* add `Arbitrary Refined` instance. ohhh this looks like a pain :(
* widen mono-traversable lower bound?
* think about reassociation, other weird utils (refined actually provides lots)
* make a little `prettyRefined :: Show a => Refined p a -> String` that reifies
  the predicate nicely and slots the value in!
  * hrmmmmm idk exactly how it'll look. maybe leave till later.

## Predicates
* `All ps`, `And` a list of predicates? My errors work for it. Seems fun.

## Fancy stuff: Logical predicate normalization
Seems awkward because we need to move between binary `kl -> kr -> Type` types,
unary `k -> Type` types, and `k` types (non-logical predicates). I got started,
but I might be going the wrong way.

I think we can only do "our best"/a heuristic approach anyway, but something is
better than nothing. I'd love to be able to "prove" that certain predicates are
actually bottom, or top.

## Predicate names: fixity
I do precedence, but not associativity. Not sure how to. `Show` doesn't help.
See: http://intrologic.stanford.edu/dictionary/operator_precedence.html

## Predicate names: precedence
An answer on this StackExchange question suggests not trying to avoid brackets
too much...
https://cs.stackexchange.com/questions/43856/operator-precedence-in-propositional-logic

I don't really agree. They also say `A v B v C` is fine because it commutes, but
that's not useful from a syntactic point of view, so I feel safer ignoring their
opinion.

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
