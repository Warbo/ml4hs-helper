{ mkDerivation, atto-lisp, attoparsec, base, bytestring, HS2AST
, stdenv, stringable, text
}:
mkDerivation {
  pname = "ML4HSHelper";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    atto-lisp attoparsec base bytestring HS2AST stringable text
  ];
  homepage = "http://chriswarbo.net/git/ml4hs-helper";
  description = "Helper functions for ML4HS";
  license = stdenv.lib.licenses.publicDomain;
}
