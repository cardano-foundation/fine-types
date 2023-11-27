{
  description = ''
    Agda tools for Haskell
    — agda, agda2hs, and suitable libraries
  '';

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";

    agda.url = "github:agda/agda?ref=v2.6.4";
    agda.inputs.nixpkgs.follows = "nixpkgs";

    agda2hs = {
      url = "github:agda/agda2hs?ref=e4bca5833042f7e18dd278e3a9eaa33fdfc8792d";
      flake = false;
    };
    agda-stdlib = {
      url = "github:agda/agda-stdlib?ref=v2.0-rc2";
      flake = false;
    };
  };

  outputs = inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
       ]; in
    inputs.flake-utils.lib.eachSystem supportedSystems (system:
      let
        # ###########################################
        # Imports

        # Packages with patches
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        agda2hs-patched = import ./patch-agda2hs.nix {
          inherit pkgs;
          agda2hs-unpatched = inputs.agda2hs;
        };

        # Library
        lib = import ./lib.nix {
          inherit (pkgs) lib writeText runCommand; 
        };
        
        # ###########################################
        # Helpers

        # Specific agda packages that we want to depend on.
        mkAgdaPackages = p: [
          (p.standard-library.overrideAttrs (oldAttrs: {
            version = "v2.0-rc2";
            src = inputs.agda-stdlib.outPath;
          }))
          (p.mkDerivation {
            pname = "agda2hs";
            version = "1.2";
            meta = "Agda-to-Haskell transpiler library";
            src = agda2hs-patched;
          })
        ];

      in rec {
        packages = {
          agda = pkgs.agda.withPackages mkAgdaPackages;
          agda2hs = pkgs.haskell.lib.doJailbreak (
            pkgs.haskellPackages.callCabal2nix "agda2hs" inputs.agda2hs.outPath {}
          );
          agda-dir =
            lib.agdaDirFromPackages (mkAgdaPackages pkgs.agdaPackages);
        };

        apps = {
          agda2hs = inputs.flake-utils.lib.mkApp {
            drv = packages.agda2hs;
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            packages.agda
            packages.agda2hs
          ];
          shellHook = ''
            export AGDA_DIR=${packages.agda-dir.outPath}
          '';
        };
      }
    ) // {
      overlay = inputs.agda.overlay;
    };
}