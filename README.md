# exhMonad

Computing exhaustification in haskell (directly inspired by https://github.com/KenyC/Exh). This is currently WIP and extremely messy!

https://github.com/patrl/hExh/blob/master/propLogic.ipynb contains a propositional fragment enriched with exhaustification operators.

https://github.com/patrl/hExh/blob/master/sauerlandAlts.ipynb contains a demonstration of how to implement Sauerland's algorithm for computing alternatives using the pointed-set monad. Exhaustification is implemented here as a special interpretation rule.

## Hacking on `exhMonad`

Currently, the easiest way to get started is via the [nix package manager](https://nixos.org/nix/).

In order to get a jupyter notebook up and running with ihaskell, simply run `nix-shell --command "jupyter lab"`. Note that this takes a very long time to build for the first time, but will be much faster on subsequent invocations.

In order to build the library, `cd` into `hExh` and run `nix-shell` followed by `cabal new-build`.
