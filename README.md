# Advent of Code 2022

My haskell attempt on [AoC 2022](https://adventofcode.com/2022).

## How to run

You can use either Nix:
```
nix run .#day1 < day1/input
```

Or Cabal directly:
```
cabal run day1 < day1/input
```

## How to hack

Use Cabal to `build`, `repl`, and `run` while developing. You'll want `cabal`, `ghc`, and (optionally) `haskell-language-server` installed.

The easiest way to get them is through Nix: `nix develop` or `nix-shell`.

Otherwise, just install them manually through your preferred package manager.
