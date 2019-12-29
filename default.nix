{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  fp-course = haskellPackages.callCabal2nix "fp-course" ./. {};

  modified-fp-course = pkgs.haskell.lib.overrideCabal fp-course (drv: {
    checkPhase = "true";
  });
in
  modified-fp-course

