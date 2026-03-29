{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;
  baseHaskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  haskellPackages = baseHaskellPackages.override {
    overrides = self: super: {
      henforcer = self.callHackage "henforcer" "1.0.0.1" {};
    };
  };
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  wikiScraper = import ./default.nix;
  drv = variant (haskellPackages.callPackage wikiScraper {});
in
pkgs.mkShell {
  buildInputs = [ pkgs.cabal-install ];
  inputsFrom = [ (if pkgs.lib.inNixShell then drv.env else drv) ];
}
