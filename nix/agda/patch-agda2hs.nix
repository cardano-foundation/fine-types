{ pkgs, agda2hs-unpatched }:

pkgs.runCommandLocal "agda2hs-patched" {} ''
  cp -r ${agda2hs-unpatched.outPath} $out
  chmod -R +w $out
  cat ${./Everything.agda} >$out/Everything.agda
''
