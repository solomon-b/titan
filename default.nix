{ mkDerivation, async, base, containers, lens, megaparsec, mtl
, parsers, stdenv, text, transformers
}:
mkDerivation {
  pname = "titan";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base containers lens megaparsec mtl parsers text transformers
  ];
  executableHaskellDepends = [
    async base containers lens megaparsec mtl parsers text transformers
  ];
  description = "A Gemini Server";
  license = stdenv.lib.licenses.gpl3;
}
