{ mkDerivation, base, containers, stdenv }:
mkDerivation {
  pname = "hExh";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ];
  testHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
