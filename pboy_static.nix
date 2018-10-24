{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc843"
, strip ? true
, makeWrapper
, symlinkJoin
, poppler_utils
}:

let

  inherit (nixpkgs) pkgs;

  pboy = { mkDerivation, base, brick, config-ini, containers
      , directory, either, filepath, hpack, microlens, microlens-th
      , path, path-io, pdfinfo, process, stdenv, text, time, titlecase
      , unordered-containers, vector, vty
      }:
      mkDerivation {
        pname = "pboy";
        version = "1.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;

        # compile statically
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        configureFlags = [
          "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
        ] ++ pkgs.lib.optionals (!strip) [
          "--disable-executable-stripping"
        ] ;

        libraryHaskellDepends = [
          base brick config-ini containers directory either filepath
          microlens microlens-th path path-io pdfinfo process text time
          titlecase unordered-containers vector vty
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base brick config-ini containers directory either filepath
          microlens microlens-th path path-io pdfinfo process text time
          titlecase unordered-containers vector vty
        ];
        preConfigure = "hpack";
        homepage = "https://github.com/2mol/pboy#readme";
        description = "a small .pdf management utility";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = pkgs.haskell.packages.${compiler};
  # haskellPackages = pkgs.haskellPackages;

  drv = haskellPackages.callPackage pboy {};

in
  symlinkJoin {
    name = "pboy-1.1.0";
    buildInputs = [makeWrapper];
    postBuild = ''
      wrapProgram "$out/bin/pboy" \
        --prefix PATH : ${poppler_utils}/bin
    '';
    paths = [drv];
  }