{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "titan";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  description = "A Gemini Server";
  license = stdenv.lib.licenses.gpl3;
}
