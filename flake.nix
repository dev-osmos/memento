{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.follows = "haskellNix/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems
      (system:
        let
          overlays = [
            haskellNix.overlay
            (final: prev: {
              memento = final.haskell-nix.hix.project { src = ./.; };
            })
          ];
          pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
          flake = pkgs.memento.flake { };
        in
        flake // {
          lib = pkgs.callPackage ./nix/lib.nix { };
          packages.default = pkgs.writeShellApplication {
            name = "mto";
            runtimeInputs = [ flake.packages."memento:exe:mto" pkgs.nix-prefetch-git ];
            text = ''
              mto "$@"
            '';
            checkPhase = "";
          };
          formatter =
            let
              inherit (pkgs.memento.tools { fourmolu = "latest"; }) fourmolu;
            in
            pkgs.writeScriptBin "fourmolu-inline" ''
              set -ex
              ${fourmolu}/bin/fourmolu -i `${pkgs.fd}/bin/fd -g *.hs src`
              ${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt `${pkgs.fd}/bin/fd -g *.nix .`
            '';
          legacyPackages = pkgs;
        }) // {
      nixosModules.default = import ./nix/module.nix;
    };

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
