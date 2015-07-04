{ mkDerivation, base, QuickCheck, stdenv, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "ML4HSHelper";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base ];
  testDepends = [ base QuickCheck tasty tasty-quickcheck ];
  homepage = "http://chriswarbo.net/git/ml4hs-helper";
  description = "Helper functions for ML4HS";
  license = stdenv.lib.licenses.publicDomain;
}
