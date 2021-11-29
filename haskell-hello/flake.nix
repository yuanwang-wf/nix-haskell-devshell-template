{
  description = "virtual environments";

  inputs.devshell.url = "github:numtide/devshell";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, flake-utils, devshell, nixpkgs }:

    let out = system:
          let pkgs = import nixpkgs {
                inherit system;
                overlays = [devshell.overlay self.overlap]  ;
              };

                 myHaskellEnv = (pkgs.haskellPackages.ghcWithHoogle (p:
          with p;
          [ haskell-hello cabal-install ghcid  ]

          ++ pkgs.haskell-hello.buildInputs));

          in {

            overlay = (final: prev: {
        haskell-hello = final.haskellPackages.callCabal2nix "haskell-hello" ./. {};
      });
            defaultPackage = pkgs.haskell-hello;
            devShell = pkgs.devshell.mkShell {
               packages = [ myHaskellEnv pkgs.nixfmt pkgs.treefmt ];
            };
          };


    in with flake-utils.lib;
      eachDefaultSystem out;
}
