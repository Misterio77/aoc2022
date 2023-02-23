{ pkgs }: let
  name = "aoc2022";
in rec {
  default = pkgs.symlinkJoin {
    inherit name;
    paths = [haskellDays rustDays];
  };
  haskellDays = pkgs.haskellPackages.developPackage {
    inherit name;
    root = ../.;
  };
  rustDays = pkgs.rustPlatform.buildRustPackage {
    inherit name;
    src = ../.;
    cargoLock.lockFile = ../Cargo.lock;
  };
}
