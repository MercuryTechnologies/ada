final: prev: {
  ada =
    final.haskell.lib.justStaticExecutables
      final.haskellPackages.ada;

  haskellPackages = prev.haskellPackages.override (old: {
    overrides =
      final.lib.fold
        final.lib.composeExtensions
        (old.overrides or (_: _: { }))
        [ (final.haskell.lib.packageSourceOverrides {
            ada = ./.;

            base16 = "1.0";

            base64 = "1.0";

          })
          (final.haskell.lib.packagesFromDirectory {
            directory = ./nix;
          })
          (hfinal: hprev: {
            cheapskate =
              final.haskell.lib.doJailbreak
                (final.haskell.lib.unmarkBroken hprev.cheapskate);

            skews =
              final.haskell.lib.dontCheck
                (final.haskell.lib.unmarkBroken hprev.skews);

            kdt =
              final.haskell.lib.dontCheck
                (final.haskell.lib.unmarkBroken hprev.kdt);

            tiktoken = final.haskell.lib.doJailbreak hprev.tiktoken;

            wss-client =
              prev.lib.pipe hprev.wss-client [
                final.haskell.lib.doJailbreak
                final.haskell.lib.dontCheck
                final.haskell.lib.unmarkBroken
              ];
          })
        ];
  });
}
