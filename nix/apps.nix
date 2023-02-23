{callPackage, ...}: let
  mkApp = pkg: name: {
    type = "app";
    program = "${pkg}/bin/${name}";
  };
  inherit (callPackage ./.) haskell rust;
in {
  day1 = mkApp haskell "day1";
  day2 = mkApp haskell "day2";
  day3 = mkApp haskell "day3";
  day4 = mkApp haskell "day4";
  day5 = mkApp rust "day5";
}
