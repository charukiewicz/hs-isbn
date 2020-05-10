let
  defaultPkgs = import (builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs-channels;
      ref = "nixos-20.03";
      rev = "4d373182597cff60b3a820affb5a73dd274e205b";
    }) {};
in
{ pkgs ? defaultPkgs }:
pkgs.haskellPackages.developPackage
  { root = ./.;
    overrides = self: super: { };
    source-overrides = { };
  }
