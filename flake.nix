{
  description = "ISBN";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem ( system:
      let
        pkgs = import nixpkgs {
                    inherit system;
                    overlays = [ ];
                    config = { allowUnfree = true; };
                };

        haskellPackages = pkgs.haskellPackages;

        packageName = "isbn";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName ./. rec {
            # Dependency overrides go here
          };

        packages.default = self.packages.${system}.${packageName};

        devShell = pkgs.lib.overrideDerivation self.packages.${system}.default.env (shellSelf: {
            # Development packages
            buildInputs = shellSelf.buildInputs ++ [
              # Necessary for the `cabal` utility
              pkgs.cabal-install

              # Necessary to rebuild the library and run
              # tests each time the source files change
              pkgs.ghcid

              # Necessary for the `make` utility
              pkgs.gnumake

              # Used to watch the source files and re-run the
              # Haddock docs build each time they change
              pkgs.entr
            ];

            shellHook = ''
                echo "Entered ${packageName} development shell"
              '';

            # Environment variables
            TMPDIR = "/tmp";
          });
      });
}
