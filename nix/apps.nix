{ pkgs }: let
  mkApp = pkg: name: {
    type = "app";
    program = "${pkg}/bin/${name}";
  };
  packages = import ./. { inherit pkgs; };
in {
  day1 = mkApp packages.haskellDays "day1";
  day2 = mkApp packages.haskellDays "day2";
  day3 = mkApp packages.haskellDays "day3";
  day4 = mkApp packages.haskellDays "day4";
  day5 = mkApp packages.rustDays "day5";
  day6 = mkApp packages.rustDays "day6";
}
