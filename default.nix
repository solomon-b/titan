{ mkDerivation, async, attoparsec, base, bytestring, containers
, lens, mtl, stdenv, text, transformers
}:
mkDerivation {
  pname = "titan";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base bytestring containers lens mtl text
    transformers
  ];
  executableHaskellDepends = [
    async attoparsec base bytestring containers lens mtl text
    transformers
  ];
  description = "A Gemini Server";
  license = stdenv.lib.licenses.gpl3;
}
