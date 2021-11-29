{
  description = "virtual environments";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.devshell.url = "github:numtide/devshell";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, devshell, flake-utils }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            devshell.overlay
            (final: prev: {
              haskell-hello =
                final.haskellPackages.callCabal2nix "haskell-hello" ./. { };
            })
          ];
        };
        myHaskellEnv = (pkgs.haskellPackages.ghcWithHoogle (p:
              with p;
              [ cabal-install ormolu hlint hpack brittany ]
           ));

      in {
        packages = { haskell-hello = pkgs.haskell-hello; };
        defaultPackage = pkgs.haskell-hello;
        checks = self.packages;
        devShell = pkgs.devshell.mkShell {
          name = "hello";
          packages = [myHaskellEnv];
        };
      });
}
