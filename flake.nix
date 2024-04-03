{ inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-23.11;

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
                  })
                  (hself: hsuper: {
                    skews =
                      self.haskell.lib.dontCheck
                        (self.haskell.lib.unmarkBroken hsuper.skews);

                    kdt = self.haskell.lib.unmarkBroken hsuper.kdt;

                    wss-client =
                      self.haskell.lib.unmarkBroken hsuper.wss-client;
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
