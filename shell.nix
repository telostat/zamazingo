{ compiler ? "ghc902"
, ...
}:

let
  ## Import sources:
  sources = import ./nix/sources.nix;

  ## Pinned nixpkgs:
  pkgs = import sources.nixpkgs { };

  ## Get the haskell set:
  haskell = pkgs.haskell.packages.${compiler};

  ## Get zamazingo:
  zamazingo = haskell.callCabal2nix "zamazingo" ./. { };

  ## Get zamazingo Haskell dependencies:
  zamazingoDeps = pkgs.haskell.lib.compose.getHaskellBuildInputs zamazingo;

  ## Get our GHC for development:
  ghc = haskell.ghcWithPackages (_: zamazingoDeps);
in
pkgs.mkShell {
  buildInputs = [
    ## Fancy stuff:
    pkgs.figlet
    pkgs.lolcat

    ## Release stuff:
    pkgs.busybox
    pkgs.gh
    pkgs.git
    pkgs.git-chglog

    ## Haskell stuff:
    ghc
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.haskell-language-server
    pkgs.haskellPackages.apply-refact
    pkgs.hlint
    pkgs.stylish-haskell
  ];

  shellHook = ''
    figlet -w 999 "ZAMAZINGO DEV SHELL" | lolcat -S 42
  '';
}
