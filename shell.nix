{ mkDerivation, base, brick, bytestring, containers, directory
, either, filepath, fmt, hpack, htoml-megaparsec, microlens
, microlens-th, pdfinfo, process, regex-compat, stdenv, tagsoup
, text, time, titlecase, unordered-containers, vector, vty, wreq
}:
mkDerivation {
  pname = "pboy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick bytestring containers directory either filepath fmt
    htoml-megaparsec microlens microlens-th pdfinfo process
    regex-compat tagsoup text time titlecase unordered-containers
    vector vty wreq
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base brick bytestring containers directory either filepath fmt
    htoml-megaparsec microlens microlens-th pdfinfo process
    regex-compat tagsoup text time titlecase unordered-containers
    vector vty wreq
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/2mol/pboy#readme";
  license = stdenv.lib.licenses.bsd3;
}
