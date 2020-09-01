{ makeWrapper
, haskellPackages
, lib
, symlinkJoin
, poppler_utils
, exiftool
}:
let
  pboy = haskellPackages.callCabal2nix "pboy" (lib.cleanSource ./.) {};
in
  symlinkJoin {
    name = "pboy-1.4";
    buildInputs = [makeWrapper];
    postBuild = ''
      wrapProgram "$out/bin/pboy" \
        --prefix PATH : ${poppler_utils}/bin
    '';
    paths = [pboy];
  }
