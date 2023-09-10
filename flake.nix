{
  description = "Advent of Code 2022";

  nixConfig = {
    extra-substituters = ["https://cache.m7.rs"];
    extra-trusted-public-keys = ["cache.m7.rs:kszZ/NSwE/TjhOcPPQ16IuUiuRSisdiIwhKZCxguaWg="];
    allow-import-from-derivation = true;
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    utils,
  }: let
    inherit (nixpkgs.lib) filter hasPrefix attrNames genAttrs any hasSuffix importTOML;
    inherit (utils.lib) filterPackages eachSystem;
    listDir = dir: attrNames (builtins.readDir dir);
    hasCabal = day: any (hasSuffix ".cabal") (listDir ./${day});
    hasCargo = day: any (n: n == "Cargo.toml") (listDir ./${day});
    # Get days dynamically (uses IFD)
    days = filter (hasPrefix "day") (listDir ./.);
    systems = ["x86_64-linux" "aarch64-linux"];
  in
    eachSystem systems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      mkRust = day: rec {
        package = let
          manifest = importTOML ./${day}/Cargo.toml;
        in pkgs.rustPlatform.buildRustPackage {
          pname = manifest.package.name;
          version = manifest.package.version;
          src = ./${day};
          cargoLock.lockFile = ./${day}/Cargo.lock;
        };
        devShell = pkgs.mkShell {
          inputsFrom = [package];
          buildInputs = with pkgs; [rustc cargo rust-analyzer clippy rustfmt];
        };
      };
      mkHaskell = day: rec {
        package = pkgs.haskellPackages.developPackage {
          root = ./${day};
        };
        devShell = pkgs.mkShell {
          inputsFrom = [package];
          buildInputs = with pkgs.haskellPackages; [ghc cabal-install haskell-language-server];
        };
      };
      mkDay = day:
        if (hasCabal day)
        then (mkHaskell day)
        else if (hasCargo day)
        then (mkRust day)
        else throw "${day} does not have a Cargo.toml nor a *.cabal file";
    in rec {
      packages = genAttrs days (day: (mkDay day).package);
      devShells = genAttrs days (day: (mkDay day).devShell);
      hydraJobs = filterPackages system packages;
      formatter = pkgs.alejandra;
    });
}
