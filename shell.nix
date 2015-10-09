{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, atto-lisp, attoparsec, base, bytestring
      , HS2AST, QuickCheck, stdenv, stringable, tasty, tasty-quickcheck
      , text
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
        testHaskellDepends = [
          atto-lisp attoparsec base bytestring HS2AST QuickCheck stringable
          tasty tasty-quickcheck text
        ];
        homepage = "http://chriswarbo.net/git/ml4hs-helper";
        description = "Helper functions for ML4HS";
        license = stdenv.lib.licenses.publicDomain;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
