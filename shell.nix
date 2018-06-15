{ mkDerivation, base, brick, containers, directory, either
, filepath, fmt, hpack, htoml-megaparsec, microlens, microlens-th
, pdfinfo, process, stdenv, text, time, titlecase
, unordered-containers, vector, vty, poppler_utils
}:
mkDerivation {
  pname = "pboy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick containers directory either filepath fmt
    htoml-megaparsec microlens microlens-th pdfinfo process text time
    titlecase unordered-containers vector vty
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base brick containers directory either filepath fmt
    htoml-megaparsec microlens microlens-th pdfinfo process text time
    titlecase unordered-containers vector vty
  ];
  buildDepends = [
    poppler_utils
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/2mol/pboy#readme";
  license = stdenv.lib.licenses.bsd3;
}
