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
    forEachSystem = nixpkgs.lib.genAttrs ["x86_64-linux" "aarch64-linux"];
    forEachPkgs = f: forEachSystem (sys: f nixpkgs.legacyPackages.${sys});
  in rec {
    packages = forEachPkgs (pkgs: import ./nix/default.nix { inherit pkgs; });
    devShells = forEachPkgs (pkgs: import ./nix/shell.nix { inherit pkgs; });
    apps = forEachPkgs (pkgs: import ./nix/apps.nix { inherit pkgs; });

    hydraJobs = packages;
    formatter = forEachPkgs (pkgs: pkgs.alejandra);
  };
}
