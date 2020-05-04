{}:
let
  pkgs = import (builtins.fetchGit {
    url = https://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-20.03";
  }) {};

  haskellPackages = pkgs.haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld: rec {
      isbn = haskellPackages.callCabal2nix "isbn" ./. {};
    };
  };

  isbn = haskellPackages.isbn;

in
{
  shell = haskellPackages.shellFor {
    packages = p: with p; [ isbn ];
    buildInputs = with haskellPackages; [
    ];
  };
}
