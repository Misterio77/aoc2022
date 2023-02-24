{
  description = "Advent of Code 2022";

  nixConfig = {
    extra-substituters = ["https://cache.m7.rs"];
    extra-trusted-public-keys = ["cache.m7.rs:kszZ/NSwE/TjhOcPPQ16IuUiuRSisdiIwhKZCxguaWg="];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
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

      apps = {
        day1 = utils.lib.mkApp {
          name = "day1";
          drv = packages.haskellDays;
        };
        day2 = utils.lib.mkApp {
          name = "day2";
          drv = packages.haskellDays;
        };
        day3 = utils.lib.mkApp {
          name = "day3";
          drv = packages.haskellDays;
        };
        day4 = utils.lib.mkApp {
          name = "day4";
          drv = packages.haskellDays;
        };
        day5 = utils.lib.mkApp {
          name = "day5";
          drv = packages.rustDays;
        };
        day6 = utils.lib.mkApp {
          name = "day6";
          drv = packages.rustDays;
        };
      };
      hydraJobs = utils.lib.filterPackages system packages;
      formatter = pkgs.alejandra;
    });
}
