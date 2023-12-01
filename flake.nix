# Nix flake based on Cardano base
# https://github.com/input-output-hk/cardano-base/blob/master/flake.nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    haskellNix.url = "github:input-output-hk/haskell.nix";
    # We need the latest nixpkgs in order to get Agda 2.6.4
    # haskellNix.inputs.nixpkgs-unstable.follows = "nixpkgs";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    iohkNix.inputs.nixpkgs.follows = "nixpkgs";
    
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
      profiling = false;
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        # not supported on ci.iog.io right now
        #"aarch64-linux"
        "aarch64-darwin"
       ]; in
    inputs.flake-utils.lib.eachSystem supportedSystems (system:
      let
        # setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
        # overlays...
        nixpkgs = import inputs.nixpkgs {
          overlays = [inputs.haskellNix.overlay]
            ++ builtins.attrValues inputs.iohkNix.overlays
            ++ [inputs.agda.overlay];
          inherit system;
          inherit (inputs.haskellNix) config;
        };
        agda2hs-patched = import ./nix/agda/patch-agda2hs.nix {
          pkgs = nixpkgs;
          agda2hs-unpatched = inputs.agda2hs;
        };

        agdapkgs =
          let
            # Library 
            lib = import ./nix/agda/lib.nix {
              inherit (nixpkgs) lib writeText runCommand; 
            };
            # Agda packages that we want to depend on.
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
          in {
            agda = nixpkgs.agda.withPackages mkAgdaPackages;
            agda2hs = nixpkgs.haskell.lib.doJailbreak (
              nixpkgs.haskellPackages.callCabal2nix "agda2hs" inputs.agda2hs.outPath {}
            );
            # Set up AGDA_DIR to point to the libraries.
            shellHook =
              let
                agda-dir =
                  lib.agdaDirFromPackages (mkAgdaPackages nixpkgs.agdaPackages);
              in ''
                export AGDA_DIR=${agda-dir.outPath}
              '';
          };

        # ... and construct a flake from the cabal.project file.
        # We use cabalProject' to ensure we don't build the plan for
        # all systems.
        flake = (nixpkgs.haskell-nix.cabalProject' rec {
          src = ./.;
          name = "fine-type";
          compiler-nix-name = "ghc928";

          # tools we want in our shell
          shell.tools = {
            cabal = "3.10.1.0";
            ghcid = "0.8.8";
            haskell-language-server = "latest";
            hlint = {};
            fourmolu = "0.13.1.0";
          };
          shell.withHoogle = true;

          shell.buildInputs = [
            nixpkgs.just
            nixpkgs.gitAndTools.git

            agdapkgs.agda
            agdapkgs.agda2hs
          ];

          shell.shellHook = agdapkgs.shellHook;
        }).flake (
          # we also want cross compilation to windows.
          nixpkgs.lib.optionalAttrs (system == "x86_64-linux") {
          crossPlatforms = p: [p.mingwW64];
        });
      in flake
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
