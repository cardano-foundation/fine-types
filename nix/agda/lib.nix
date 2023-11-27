# Library of Nix functions

{ lib, runCommand, writeText } :

{
  # agdaDirFromPackages : List AgdaPackage -> Derivation
  #
  # Collate a list of packages into a `libraries` file.
  # Returns a derivation for a directory that can be used as AGDA_DIR.
  # 
  # Inspired by
  # https://github.com/NixOS/nixpkgs/blob/5b5005879388f32bb639cb389635a99cde4ea000/pkgs/build-support/agda/default.nix#L8-L36
  agdaDirFromPackages = pkgs :
    let
      libraries-file = writeText "libraries" ''
        ${(lib.strings.concatMapStringsSep "\n" (p: "${p}/${p.libraryFile}") pkgs)}
      '';
      pname = "agda-libraries";
      version = "1.0";
    in
      runCommand "${pname}-${version}" {
        inherit pname version;
      } ''
        mkdir -p $out
        cp ${libraries-file} $out/libraries
      '';
}
