{ inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-23.11;

    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    let
      overlay = import ./overlay.nix;

    in
      utils.lib.eachDefaultSystem (system:
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

            devShells.default = pkgs.haskellPackages.ada.env;
          }
      ) // {
        overlays.default = overlay;

        nixosModules.default = import ./ada.nix;
      };
}
