{ pkgs, ... }:
pkgs.mkShell {
  inputsFrom = [
    (pkgs.callPackage ./default.nix { })
  ];
  buildInputs = with pkgs; [
    haskell-language-server
    cabal-install
    ghc
  ];
}
