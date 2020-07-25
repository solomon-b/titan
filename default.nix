{ mkDerivation, async, attoparsec, base, bytestring, containers
, lens, mtl, network, network-simple-tls, stdenv, text, tls
, transformers, x509, x509-store
}:
mkDerivation {
  pname = "titan";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base bytestring containers lens mtl network
    network-simple-tls text tls transformers x509 x509-store
  ];
  executableHaskellDepends = [
    async attoparsec base bytestring containers lens mtl network
    network-simple-tls text tls transformers x509 x509-store
  ];
  description = "A Gemini Server";
  license = stdenv.lib.licenses.gpl3;
}
