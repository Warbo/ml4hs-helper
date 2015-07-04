{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, atto-lisp, attoparsec, base, bytestring
      , QuickCheck, stdenv, stringable, tasty, tasty-quickcheck, text
      }:
      mkDerivation {
        pname = "ML4HSHelper";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          atto-lisp attoparsec base bytestring stringable text
        ];
        testDepends = [
          atto-lisp attoparsec base bytestring QuickCheck stringable tasty
          tasty-quickcheck text
        ];
        homepage = "http://chriswarbo.net/git/ml4hs-helper";
        description = "Helper functions for ML4HS";
        license = stdenv.lib.licenses.publicDomain;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
