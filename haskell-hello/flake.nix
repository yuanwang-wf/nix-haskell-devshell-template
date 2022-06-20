{
  description = "A Hello World in Haskell with a dependency and a devShell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ ];
        };

        t = pkgs.lib.trivial;
        hl = pkgs.haskell.lib;

        name = "haskell-hello";

        wireShellhook = haskellPackage:
          hl.overrideCabal haskellPackage (oldAttributes: {
            shellHook = (oldAttributes.shellHook or "") + self.checks.${system}.pre-commit-check.shellHook;
          });
        project = devTools: # [1]
          let addBuildTools = (t.flip hl.addBuildTools) devTools;
          in
          pkgs.haskellPackages.developPackage {
            root = nixpkgs.lib.sourceFilesBySuffices ./. [
              ".cabal"
              ".hs"
              "package.yaml"
            ];
            name = name;
            returnShellEnv = !(devTools == [ ]); # [2]
            modifier = (t.flip t.pipe) [

              addBuildTools
              wireShellhook
              hl.dontHaddock
              hl.enableStaticLibraries
              hl.justStaticExecutables
              # hl.disableLibraryProfiling
              # hl.disableExecutableProfiling
            ];
          };

      in
      {
        packages.pkg = project [ ]; # [3]
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              ormolu.enable = true;
            };
          };
        };
        defaultPackage = self.packages.${system}.pkg;
        devShell = project (with pkgs.haskellPackages; [
          # [4]
          cabal-fmt
          cabal-install
          haskell-language-server
          hlint
          ormolu
        ]);

      });
}
