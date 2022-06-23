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
          spago = prev.spago.overrideAttrs (o: rec {
            src = prev.fetchgit {
              url = "https://github.com/yuanwang-wf/spago.git";
              sha256 = "10wy61zc236jzpngmw1qplcbfkjcjdk2aifzkbm697gqpks2vskd";
              rev = "5d195e13cecdd2a3a91f3bca687f54e71ba6e463";
              fetchSubmodules = true;
            };
          });
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        easy-ps = import easy-ps-nix { inherit pkgs; };
        inherit (pkgs) spago;
        inherit (easy-ps) psa purescript-language-server purs purs-tidy spago2nix;

        t = pkgs.lib.trivial;
        hl = pkgs.haskell.lib;

        name = "servant-halogen-hello";
        purs-tidy-hook = {
          enable = true;
          name = "purs-tidy";
          entry = "${purs-tidy}/bin/purs-tidy format-in-place";
          files = "\\.purs$";
          language = "system";
        };
        frontendJs = (import ./frontends { inherit pkgs purs spago; }).frontendJs;
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
          purescript-language-server
          purs
          purs-tidy
          spago
          spago2nix
        ]);

      });
}
