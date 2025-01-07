{
  description = "Automated Verification of Tensor Graph Rewrites";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hPkgs = pkgs.haskell.packages."ghc9101".extend (hself: hsuper: {
          hlint = hself.callCabal2nix "hlint"
            (pkgs.fetchFromGitHub {
              owner = "ndmitchell";
              repo = "hlint";
              rev = "7dfba720eaf6fa9bd0b23ae269334559aa722847";
              sha256 = "sha256-niGBdSrkatr+TZCcLYXo4MDg5FyXTYiKQ5K+ZIWSWBs=";
            })
            { };
        });
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

        devTools = with pkgs; [
          z3
          stack-wrapped
          hPkgs.hlint
          hPkgs.haskell-language-server
          (cvc5.overrideAttrs (oldAttrs: rec {
            cmakeFlags = oldAttrs.cmakeFlags ++ [
              "-DUSE_POLY=ON"
            ];
            buildInputs = oldAttrs.buildInputs ++ [
              libpoly
            ];
          }))
          (python3.withPackages (ps: with ps; [
            numpy
            matplotlib
          ]))
        ];
      in
      {
        formatter = pkgs.nixpkgs-fmt;
        devShell = pkgs.mkShell {
          buildInputs = devTools;

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
        };
      });
}
