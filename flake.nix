{
  description = "Advent of Code 2022";

  nixConfig = {
    extra-substituters = ["https://cache.m7.rs"];
    extra-trusted-public-keys = ["cache.m7.rs:kszZ/NSwE/TjhOcPPQ16IuUiuRSisdiIwhKZCxguaWg="];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    name = "aoc2022";
    forEachSystem = nixpkgs.lib.genAttrs ["x86_64-linux" "aarch64-linux"];
    forEachPkgs = f: forEachSystem (sys: f nixpkgs.legacyPackages.${sys});
    mkApp = pkg: name: {
      type = "app";
      program = "${pkg}/bin/${name}";
    };
  in rec {
    packages = forEachPkgs (pkgs: rec {
      default = pkgs.symlinkJoin {
        inherit name;
        paths = [ haskell rust ];
      };
      haskell = pkgs.haskellPackages.developPackage {
        inherit name;
        root = ./.;
      };
      rust = pkgs.rustPlatform.buildRustPackage {
        inherit name;
        src = ./.;
        cargoLock.lockFile = ./Cargo.lock;
      };
    });

    devShells = forEachPkgs (pkgs: rec {
      default = pkgs.mkShell {
        inputsFrom = [ haskell rust ];
      };
      haskell = pkgs.mkShell {
        inputsFrom = [ packages.${pkgs.system}.haskell ];
        buildInputs = with pkgs; [
          ghc
          cabal-install
          haskell-language-server
        ];
      };
      rust = pkgs.mkShell {
        inputsFrom = [ packages.${pkgs.system}.rust ];
        buildInputs = with pkgs; [
          rustc
          cargo
          rust-analyzer
          clippy
        ];
      };
    });

    apps = forEachPkgs (
      pkgs: let
        inherit (packages.${pkgs.system}) haskell rust;
      in {
        day1 = mkApp haskell "day1";
        day2 = mkApp haskell "day2";
        day3 = mkApp haskell "day3";
        day4 = mkApp haskell "day4";
        day5 = mkApp rust "day5";
      }
    );

    hydraJobs = packages;
    formatter = forEachPkgs (pkgs: pkgs.alejandra);
  };
}
