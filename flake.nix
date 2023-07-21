{
  description = "Advent of Code 2022";

  nixConfig = {
    extra-substituters = ["https://cache.m7.rs"];
    extra-trusted-public-keys = ["cache.m7.rs:kszZ/NSwE/TjhOcPPQ16IuUiuRSisdiIwhKZCxguaWg="];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    utils,
  }:
    utils.lib.eachSystem ["x86_64-linux" "aarch64-linux"] (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in rec {
      packages = {
        default = pkgs.symlinkJoin {
          name = "aoc2022";
          paths = with packages; [haskellDays rustDays];
        };
        haskellDays = pkgs.haskellPackages.developPackage {
          name = "aoc2022";
          root = ./.;
        };
        rustDays = pkgs.rustPlatform.buildRustPackage {
          name = "aoc2022";
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;
        };
      };

      devShells = {
        default = pkgs.mkShell {
          inputsFrom = with devShells; [haskellShell rustShell];
        };
        haskellShell = pkgs.mkShell {
          inputsFrom = with packages; [haskellDays];
          buildInputs = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
          ];
        };
        rustShell = pkgs.mkShell {
          inputsFrom = with packages; [rustDays];
          buildInputs = with pkgs; [
            rustc
            cargo
            rust-analyzer
            clippy
            rustfmt
          ];
        };
      };

      apps = let
        mkApp' = name: drv: utils.lib.mkApp { inherit name drv; };
      in {
        day1 = mkApp' "day1" packages.haskellDays;
        day2 = mkApp' "day2" packages.haskellDays;
        day3 = mkApp' "day3" packages.haskellDays;
        day4 = mkApp' "day4" packages.haskellDays;
        day5 = mkApp' "day5" packages.rustDays;
        day6 = mkApp' "day6" packages.rustDays;
        day25 = mkApp' "day25" packages.rustDays;
      };
      hydraJobs = utils.lib.filterPackages system packages;
      formatter = pkgs.alejandra;
    });
}
