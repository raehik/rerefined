# rerefined to-dos
* add tests
* widen mono-traversable lower bound?
* clarify pretty predicates
  * logical uses symbols from propositional logic because that's closest to what
    they are (using boolean operators would make you think you're dealing with
    booleans, which you aren't)
  * but the operators are boolean because they have easily-accessible symbols...
    lol
* think about reassociation, other weird utils (refined actually provides lots)
* make a little `prettyRefined :: Show a => Refined p a -> String` that reifies
  the predicate nicely and slots the value in!
  * hrmmmmm idk exactly how it'll look. maybe leave till later.

## Predicates
* `All ps`, `And` a list of predicates? My errors work for it. Seems fun.

## Predicate normalization
I've got basic logical predicate normalization. It seems adding further
normalization will be more complicated. Relational normalization seems to
rely on inspecting logical predicates as well e.g. `(< n) && (> m) where n > m =
_|_`. I'm fairly certain we have to take a heuristic approach, and I'm fairly
happy with leaving this for a while.

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

## Instances
### Avoid
* I think the aeson instances for `Refined` aren't good... I might have a
  predicate that wants to alter JSON schema but now I have to re-newtype?

## Terminology
* read papers
  * https://arxiv.org/pdf/2010.07763.pdf
    * arxiv can break idk. https://arxiv.org/abs/2010.07763
  * https://prl.khoury.northeastern.edu/blog/static/refinement_types_lecture.pdf
  * https://www.google.com/search?q=refinement+types+paper
