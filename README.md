# rerefined
[refined-nv-gh]:      https://github.com/nikita-volkov/refined
[refined-nv-hackage]: https://hackage.haskell.org/package/refined
[strongweak-hackage]: https://hackage.haskell.org/package/strongweak
[binrep-hackage]:     https://hackage.haskell.org/package/binrep
[refined1-hackage]:   https://hackage.haskell.org/package/refined1

Rewrite of Nikita Volkov's [refined][refined-nv-hackage] library.

* same concept
* same performance
* more instances
* better ergonomics (no insidious `Typeable` constraints)
* internals: fewer dependencies (no `aeson`), better errors, more concise

## Why?
I used the original refined library fairly extensively to power other libraries
(see [strongweak][strongweak-hackage], [binrep][binrep-hackage]), though I moved
to a fork [refined1][refined1-hackage] some time ago to provide a feature I
needed. I think the library has some flaws and I want to contribute, but my
tiny tweaks are still pending. A good excuse to rewrite from the ground up.

That said, all source code is original.

## License
Provided under the MIT license. See `LICENSE` for license text.
