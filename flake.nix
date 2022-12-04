{
  description = "Advent of Code 2022";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      mkApp = pkg: name: { type = "app"; program = "${pkg}/bin/${name}"; };
    in
    rec {
      packages.default = pkgs.haskellPackages.callCabal2nix "aoc2022" ./. { };

      devShells.default = pkgs.mkShell {
        inputsFrom = [ packages.default ];
        buildInputs = with pkgs; [
          haskell-language-server
          cabal-install
          ghc
        ];
      };

      apps = {
        day1 = mkApp packages.default "day1";
        day2 = mkApp packages.default "day2";
        day3 = mkApp packages.default "day3";
        day4 = mkApp packages.default "day4";
      };
    }
  );
}
