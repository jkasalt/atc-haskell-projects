# default.nix
{
  pkgs ? import <nixpkgs> { },
}:

pkgs.haskellPackages.developPackage {
  name = "tic-tac-toe";
  root = ./.;
}
