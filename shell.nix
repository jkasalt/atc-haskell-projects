# shell.nix
{
  pkgs ? import <nixpkgs> { },
  ...
}:
pkgs.mkShell {
  nativeBuildInputs = builtins.attrValues {
    inherit (pkgs.haskellPackages)
      haskell-language-server
      ghc
      cabal-install
      ghcid
      ;
  };

  shellHook = ''
    cabal update
  '';
}
