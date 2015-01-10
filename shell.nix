let
  pkgs = import <nixpkgs> {};

  hpkgs = pkgs.haskellngPackages;

  pkg = (hpkgs.callPackage (import ./.) {
    inherit (pkgs) stdenv;
  }).overrideScope hpkgs.amazonkaEnv;
in pkg.env
