# rerefined
[refined-nv-gh]:      https://github.com/nikita-volkov/refined
[refined-nv-hackage]: https://hackage.haskell.org/package/refined
[strongweak-hackage]: https://hackage.haskell.org/package/strongweak
[binrep-hackage]:     https://hackage.haskell.org/package/binrep
[refined1-hackage]:   https://hackage.haskell.org/package/refined1

Rewrite of Nikita Volkov's [refined][refined-nv-hackage] library.

* same concept
* same performance
* more predicates (signed value comparisons)
* more features (functor predicates, predicate simplification)
* better ergonomics (no insidious `Typeable` constraints)
* neater internals: fewer dependencies (no `aeson`), better errors, more concise

## Why?
I used the original [refined][refined-nv-hackage] library fairly extensively to
power other libraries (see [strongweak][strongweak-hackage],
[binrep][binrep-hackage]), though I moved to a fork [refined1][refined1-hackage]
some time ago to provide a feature I needed. I think the library has some flaws
and I want to contribute, but my tiny tweaks are still pending after a few
years. A good excuse to rewrite from the ground up.

All source code is original.

### Why not merge into refined?
rerefined is a complete modern rewrite. As such, it works plenty differently,
and doesn't support older GHCs.

I do want to add missing features into refined, such as functor predicates. But
refined is receiving minimal maintenance recently (2022-2024), and my changesets
are languishing.

## Differences from original refined
### New features
#### `Refined1` for functor predicates
[refined-gh-refined1]: https://github.com/nikita-volkov/refined/pull/99

Some predicates operate on the `f` of an `f a`. In such cases, given an
`Refined1 (f a) :: Refined1 p f a`, we should be permitted to make changes
inside the functor, that do not change the functor structure/shape. These are
provided by the `Functor` and `Traversable` instances on `Refined1`.

Draft PR for refined at [refined#99][refined-gh-refined1].

#### Type-level refinement simplification
A primitive predicate simplifier is provided at `Rerefined.Simplify`. It can
simplify immediate cases of identity laws and so on.

#### Infix operator-style predicate definitions
`Rerefined.Predicate.Operators` exports type synonyms for writing predicates
using infix operators. (refined provides a handful, but they coincide with
existing operators, such as `&&`.)

### Changes
#### Simplified errors
refined encoded the logical predicates in its error type. This doesn't enable
any further analysis, just turns a non-sum type into a sum type and complicates
consumption. Furthermore, this error type is first transformed into another
recursive ADT, which is only then pretty printed. This is unnecessary (even
mentioned in the code).

rerefined has a single-constructor error type which can be easily and
efficiently turned into a string-like in a single pass.

#### No insidious `Typeable` contexts
[hackage-tls]: https://hackage.haskell.org/package/type-level-show

See [refined#101](https://github.com/nikita-volkov/refined/issues/101).
`Typeable` is useful, but the way it is used brings lots of `Typeable` contexts.

rerefined asks that you do a bit more work upfront, but gives you tools and
grants much more power. Predicates now must declare a "name":

```haskell
class Predicate p where type PredicateName (d :: Natural) p :: Symbol
```

Precedence is supported to permit writing names as infix operators. (For now,
the logical operators primarily look like their propositional logic
counterparts.) The [type-level-show][hackage-tls] package provides utilities for
printing `Natural`s and handling precedence.

#### More re-use
What do `LessThan`, `GreaterThan`, `EqualTo` etc. have in common? They're all
relational binary operators where one value is a pre-filled `Natural`. rerefined
packs all of these into a single predicate that takes a type-level relational
operator. Only one instance for the same amount of code, and much easier to
reason about.

We take this even further and allow passing a type-level sign, to enable
comparing negative values.

We take this _even_ further and use the same relational operator definitions to
define length comparisons, where the other value is taken from the input's
length (rather than its numeric value). This does not take a sign, since length
must be non-negative.

#### More instances
You know that length comparison predicate above? It has a _single instance_ for
each of `Refined1` and `Refined`:

```haskell
-- | Compare the length of a 'Foldable' to a type-level 'Natural' using the
--   given 'RelOp'.
instance (KnownNat n, Foldable f, ReifyRelOp op)
  => Refine1 (CompareLength op n) f where
    validate1 p = validateCompareLength p . length

-- | Compare the length of a 'MonoFoldable' to a type-level 'Natural' using the
--   given 'RelOp'.
instance (KnownNat n, MonoFoldable a, ReifyRelOp op)
  => Refine (CompareLength op n) a where
    validate p = validateCompareLength p . olength
```

We get a ton more instances for a ton less code. (Note that mono-foldable has a
surprisingly small footprint, as most of its transitive dependencies are core
libraries.)

### Other
* much cleaner module organization
  * predicates defined in standalone modules; only import what you need
* `Rerefined.Predicate.Via.validateVia` for defining predicates that are
  implemented by an existing predicate
* more logical predicates (NAND, if, if-and-only-if)

## License
Provided under the MIT license. See `LICENSE` for license text.
