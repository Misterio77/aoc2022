{symlinkJoin, haskellPackages, rustPlatform, ...}: let
  name = "aoc2022";
in rec {
  default = symlinkJoin {
    inherit name;
    paths = [haskell rust];
  };
  haskell = haskellPackages.developPackage {
    inherit name;
    root = ../.;
  };
  rust = rustPlatform.buildRustPackage {
    inherit name;
    src = ../.;
    cargoLock.lockFile = ../Cargo.lock;
  };
}
