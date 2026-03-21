{ mkDerivation, aeson, base, bytestring, containers, lens, lib
, modern-uri, network-uri, parsec, parser-combinators, text
, transformers, pkgs
, scrappy-core ? pkgs.haskellPackages.callPackage (pkgs.fetchFromGitHub {
    owner = "Ace-Interview-Prep";
    repo = "scrappy-core";
    rev = "913670f2f83cabb2b56302e17604ec488e89da7b";
    sha256 = "sha256-qA6+5Lxsw5q0DWYNx0VnvCmYo2f2/oKvDPVH0ZNgfXc=";
}) {}
}:
mkDerivation {
  pname = "scrappy-template";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers lens modern-uri network-uri parsec
    parser-combinators text transformers scrappy-core
  ];
  homepage = "https://github.com/Ace-Interview-Prep/scrappy";
  description = "html pattern matching library and high-level interface concurrent requests lib for webscraping";
  license = lib.licenses.bsd3;
}
