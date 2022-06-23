{ pkgs, purs, spago }:

let spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

in
{
  # https://github.com/cideM/lions-backend/blob/main/client/default.nix#L40
  frontendJs = pkgs.stdenv.mkDerivation {
    name = "frontendJs";
    buildInputs = [ spagoPkgs.installSpagoStyle spagoPkgs.buildSpagoStyle ];
    nativeBuildInputs = with pkgs; [ purs spago esbuild ];
    src = ./.;
    unpackPhase = ''
      cp $src/spago.dhall .
      cp $src/packages.dhall .
      cp -r $src/src .
      install-spago-style
    '';
    # https://github.com/purescript/spago/blob/310a7096b0d3e86f6464c667aa2339045d85d505/src/Spago/Build.hs#L422
    # https://esbuild.github.io/api/#transform-api
    buildPhase = ''
      build-spago-style "./src/**/*.purs"
      ${pkgs.spago}/bin/spago -V bundle-app --no-install --main Main --no-build --global-cache skip
    '';
    installPhase = ''
      mkdir $out
      mv frontend.js $out/
    '';
  };

}
