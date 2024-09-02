{ inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat/v1.0.0";

      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils/v1.0.0";

    nixpkgs.url = "github:NixOS/nixpkgs/24.05";
  };

  outputs = { flake-utils, nixpkgs, ... }:
    let
      overlay = import ./overlay.nix;

    in
      flake-utils.lib.eachDefaultSystem (system:
        let
          config = { };

          pkgs =
            import nixpkgs { inherit config system; overlays = [ overlay ]; };

        in
          rec {
            packages.default = pkgs.haskellPackages.ada;

            apps.default = {
              type = "app";

              program = "${pkgs.ada}/bin/ada";
            };

            devShells.default =
              pkgs.mkShell {
                inputsFrom = [ pkgs.haskellPackages.ada.env ];

                packages = [
                  pkgs.cabal-install
                  pkgs.ghcid
                ];
              };
          }
      ) // {
        overlays.default = overlay;

        nixosModules.default = import ./ada.nix;
      };
}
