let
  pkgs = import ../nixpkgs.nix {};
  hpkgs = pkgs.haskell.packages.ghc902;
  aoc = hpkgs.callCabal2nix "aoc" ./. {};

in
  hpkgs.shellFor { packages = ps : [ aoc ]; }
