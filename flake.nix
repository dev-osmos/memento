{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    stacklock2nix.url = "github:cdepillabout/stacklock2nix";
    all-cabal-hashes = {
      url = "github:commercialhaskell/all-cabal-hashes/hackage";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, stacklock2nix, all-cabal-hashes }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgs = system: import nixpkgs {
        inherit system;
        overlays = [
          stacklock2nix.overlay
          self.overlays.default
        ];
      };
    in
    {
      overlays.default = final: prev: {
        memento = final.memento-pkgSet.pkgSet.memento;
        memento-pkgSet = final.stacklock2nix {
          stackYaml = ./stack.yaml;
          stackYamlLock = ./stack.yaml.lock;
          baseHaskellPkgSet = final.haskell.packages.ghc92;
          additionalHaskellPkgSetOverrides = hfinal: hprev: {
            mkDerivation = a: hprev.mkDerivation (a // { doCheck = false; doHaddock = false; });
          };
          additionalDevShellNativeBuildInputs = stacklockHaskellPkgSet: [
            final.cabal-install
            final.haskell.packages.ghc92.haskell-language-server
          ];
          inherit all-cabal-hashes;
        };
      };
      lib = forAllSystems (system: (pkgs system).callPackage ./nix/lib.nix { });
      nixosModules.default = import ./nix/module.nix;
      packages = forAllSystems (system: {
        default = self.packages."${system}".memento;
        memento-unwrapped = (pkgs system).memento;
        memento = (pkgs system).writeShellApplication {
          name = "mto";
          runtimeInputs = [ self.packages."${system}".memento-unwrapped (pkgs system).nix-prefetch-git ];
          text = ''
            mto "$@"
          '';
          checkPhase = "";
        };
      });
      devShells = forAllSystems (system: {
        default = (pkgs system).memento-pkgSet.devShell.overrideAttrs (a: {
          shellHook =
            (a.shellHook or "") +
            ''
              export NIX_PATH=nixpkgs=${nixpkgs}
            '';
        });
        stack = (pkgs system).mkShell {
          packages = [ (pkgs system).stack ];
          shellHook =
            ''
              export NIX_PATH=nixpkgs=${nixpkgs}
            '';
        };
      });
      formatter = forAllSystems (system:
        let
          inherit (pkgs system) fd haskell nixpkgs-fmt writeScriptBin;
          inherit (haskell.packages.ghc928) fourmolu;
        in
        writeScriptBin "fourmolu-inline" ''
          set -ex
          ${fourmolu}/bin/fourmolu -i `${fd}/bin/fd -g *.hs src`
          ${nixpkgs-fmt}/bin/nixpkgs-fmt `${fd}/bin/fd -g *.nix .`
        '');
      legacyPackages = forAllSystems pkgs;
    };
}
