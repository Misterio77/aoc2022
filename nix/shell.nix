{ pkgs }:
let
  packages = import ./. { inherit pkgs; };
in rec {
  default = pkgs.mkShell {
    inputsFrom = [haskellShell rustShell];
  };
  haskellShell = pkgs.mkShell {
    inputsFrom = [ packages.haskellDays ];
    buildInputs = with pkgs; [
      ghc
      cabal-install
      haskell-language-server
    ];
  };
  rustShell = pkgs.mkShell {
    inputsFrom = [ packages.rustDays ];
    buildInputs = with pkgs; [
      rustc
      cargo
      rust-analyzer
      clippy
    ];
  };
}
