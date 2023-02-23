{
  mkShell,
  callPackage,
  ghc,
  cabal-install,
  haskell-language-server,
  rustc,
  cargo,
  rust-analyzer,
  clippy,
  ...
}: rec {
  default = mkShell {
    inputsFrom = [haskellShell rustShell];
  };
  haskellShell = mkShell {
    inputsFrom = [(callPackage ./.).haskell];
    buildInputs = [
      ghc
      cabal-install
      haskell-language-server
    ];
  };
  rustShell = mkShell {
    inputsFrom = [(callPackage ./.).rust];
    buildInputs = [
      rustc
      cargo
      rust-analyzer
      clippy
    ];
  };
}
