{ nixpkgs ? import <nixpkgs> {}, compiler ? "default"}:
let
  inherit (nixpkgs) pkgs;
  drv = import ./default.nix { inherit nixpkgs compiler; };
  drvWithTools = pkgs.haskell.lib.addBuildDepends drv (with pkgs;
  [ cabal-install
    hlint
    haskellPackages.brittany
  ]);
in
  if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
