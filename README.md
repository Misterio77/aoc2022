[![Built with Nix](https://img.shields.io/static/v1?logo=nixos&logoColor=white&label=&message=Built%20with%20Nix&color=41439a)](https://builtwithnix.org)
[![Hydra Status](https://img.shields.io/endpoint?url=https://hydra.m7.rs/job/aoc2022/main/x86_64-linux.default/shield)](https://hydra.m7.rs/jobset/aoc2022/main#tabs-jobs)

# Advent of Code 2022

My Haskell + Rust attempt on [AoC 2022](https://adventofcode.com/2022).

## How to run

```
nix run .#day1
```

## How to hack

Use Cabal and Cargo to `build`, and `run` while developing. You'll want:
- Haskell: `cabal`, `ghc`, and (optionally) `haskell-language-server`
- Rust: `cargo`, `rustc`, and (optionally) `rust-analyzer` and `clippy`

Nix devShells are available for each and every day, for example:

```
nix develop .#day1
```

Otherwise, just install them manually through your preferred package manager.
