{ inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-22.11;

    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        config = { };

        overlay = self: super: {
          ada =
            self.haskell.lib.justStaticExecutables
              self.haskellPackages.ada;

          haskellPackages = super.haskellPackages.override (old: {
            overrides =
              pkgs.lib.fold
                pkgs.lib.composeExtensions
                (old.overrides or (_: _: { }))
                [ (self.haskell.lib.packageSourceOverrides {
                    ada = ./.;

                    openapi-slack = ~/proj/openapi-slack;
                  })
                  (hself: hsuper: {
                    openai-servant =
                      self.haskell.lib.unmarkBroken hsuper.openai-servant;

                    openai-hs = self.haskell.lib.dontCheck hsuper.openai-hs;
                  })
                ];
          });
        };

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
    );
}
