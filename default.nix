{ compiler ? "ghc902"
, ...
}:

let
  ## Import sources:
  sources = import ./nix/sources.nix;

  ## Import nixpkgs:
  pkgs = import sources.nixpkgs { };

  ## Get the haskell set:
  haskell = pkgs.haskell.packages.${compiler};
in
haskell.callCabal2nix "zamazingo" ./. { }
