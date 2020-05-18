let
  # Default set of pinned packages
  defaultPkgs = import (
    builtins.fetchGit
      (builtins.fromJSON (builtins.readFile ./nixpkgs.json))
    ) {};

  # Extra packages to be available in the shell environment
  extraBuildInputs = pkgs: [
      # Necessary for the `cabal` utility
      pkgs.cabal-install

      # Necessary to rebuild the library and run
      # tests each time the source files change
      pkgs.ghcid

      # Necessary for the `make` utility
      pkgs.cmake

      # Used to watch the source files and re-run the
      # Haddock docs build each time they change
      pkgs.entr
    ];

  mkPackage = pkgs: (
      pkgs.haskellPackages.developPackage
        { root = ./.;
          overrides = self: super: { };
          source-overrides = { };
        }
    ).overrideAttrs(attrs: {
      buildInputs = attrs.buildInputs ++ (extraBuildInputs pkgs);
    });
in
{ pkgs ? defaultPkgs }:
mkPackage pkgs
