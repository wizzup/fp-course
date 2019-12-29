{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;
  drv = import ./default.nix { inherit nixpkgs compiler; };
  drvWithTools = pkgs.haskell.lib.addBuildDepends drv [
    pkgs.haskellPackages.doctest
    pkgs.haskellPackages.ghcid
  ];
in
 drvWithTools
