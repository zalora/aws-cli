{ mkDerivation, amazonka, amazonka-cloudwatch, base, lens
, optparse-applicative, resourcet, stdenv, text
}:
mkDerivation {
  pname = "aws-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    amazonka amazonka-cloudwatch base lens optparse-applicative
    resourcet text
  ];
  homepage = "https://github.com/zalora/aws-cli";
  license = stdenv.lib.licenses.mpl20;
}
