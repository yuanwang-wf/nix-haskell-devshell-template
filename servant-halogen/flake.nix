{
  description = "A Hello World in Haskell with a dependency and a devShell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    easy-ps-nix = {
      url = "github:justinwoo/easy-purescript-nix/master";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks, easy-ps-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = final: prev: {
          inherit (final.callPackage easy-ps-nix { }) psa purescript-language-server purs purs-tidy spago spago2nix;
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };

        t = pkgs.lib.trivial;
        hl = pkgs.haskell.lib;

        name = "servant-halogen-hello";
        purs-tidy-hook = {
          enable = true;
          name = "purs-tidy";
          entry = "${pkgs.purs-tidy}/bin/purs-tidy format-in-place";
          files = "\\.purs$";
          language = "system";
        };
        frontendJs = (import ./frontends { inherit pkgs; }).frontendJs;
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
        runSite = pkgs.runCommand "hello-website" { PORT = "9000"; "STATIC_FILE_PATH" = "${frontendJs}"; }
          ''
            ${self.packages.${system}.pkg}/bin/haskell-hello
          ''
        ;
      in
      {
        packages = flake-utils.lib.flattenTree {
          frontendJs = frontendJs;
          pkg = project [ ];
        };
        apps.runSite = flake-utils.lib.mkApp { drv = runSite; };
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt = {
                enable = true;
                excludes = [ ".*spago-packages.nix$" ];
              };
              inherit purs-tidy-hook;
              ormolu.enable = true;
            };
          };
        };
        defaultPackage = self.packages.${system}.pkg;
        devShell = project (with pkgs.haskellPackages; [
          # [4]
          cabal-fmt
          cabal-install
          warp
          haskell-language-server
          hlint
          ormolu
          pkgs.esbuild
          pkgs.purescript-language-server
          pkgs.purs
          pkgs.purs-tidy
          pkgs.spago
          pkgs.spago2nix
        ]);

      });
}
