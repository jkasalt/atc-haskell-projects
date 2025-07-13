{
  description = "Description for the project";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.treefmt-nix.flakeModule ];
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem =
        { pkgs, ... }:
        {
          treefmt.programs = {
            nixfmt.enable = true;
            fourmolu.enable = true;
            cabal-gild.enable = true;
            hlint.enable = true;
            statix.enable = true;
            deadnix.enable = true;
          };
          packages.default = pkgs.callPackage ./default.nix { };
          devShells.default = pkgs.callPackage ./shell.nix { };
        };
    };
}
