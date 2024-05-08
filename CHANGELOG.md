## 0.3.0 (2024-05-07)
* refactor predicate names: now handled with a sort of type-level `Show`. no
  `Typeable`, lots of custom prettiness (infix operators!)
* refactor logical predicates, keeping them together doesn't help
* define relational operators using `Ordering` and logical `Or`
* add missing `Foldable`, `Traversable` instances to `Refined1`
* general cleanup

## 0.2.0 (2024-05-01)
* add missing `Show` instance to `RefineFailure`
* add missing `Functor` instance to `Refined1`
* general cleanup

## 0.1.0 (2024-04-30)
Initial release.

* rewrite of Nikita Volkov's refined library
