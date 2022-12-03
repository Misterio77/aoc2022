{
  description = "Foo Bar Haskell Project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgsFor = forAllSystems (system: nixpkgs.legacyPackages.${system});
      mkApp = pkg: bin: { type = "app"; program = "${pkg}/bin/${bin}"; };
    in
    rec {
      packages = forAllSystems (system: {
        default = pkgsFor.${system}.callPackage ./default.nix { };
      });

      apps = forAllSystems (system: {
        day1 = mkApp packages.${system}.default "day1";
        day2 = mkApp packages.${system}.default "day2";
      });

      devShells = forAllSystems (system: {
        default = pkgsFor.${system}.callPackage ./shell.nix { };
      });

      hydraJobs = packages;
    };
}
