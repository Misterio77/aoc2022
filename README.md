# Advent of Code 2022

My Haskell + Rust attempt on [AoC 2022](https://adventofcode.com/2022).

## How to run

### Nix (Haskell or Rust)

```
nix run .#day1
```

### Cabal (Haskell only)

```
cd haskell
cabal run day1
```

### Cargo (Rust only)

```
cd haskell
cargo run day5
```

## How to hack

Use Cabal and Cargo to `build`, and `run` while developing. You'll want:
- Haskell: `cabal`, `ghc`, and (optionally) `haskell-language-server`
- Rust: `cargo`, `rustc`, and (optionally) `rust-analyzer` and `clippy`

The easiest way to get them is through Nix: `nix develop .#rust` or `nix develop .#haskell`.

Otherwise, just install them manually through your preferred package manager.
