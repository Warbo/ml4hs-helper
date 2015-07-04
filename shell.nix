{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, QuickCheck, stdenv, tasty
      , tasty-quickcheck
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
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
