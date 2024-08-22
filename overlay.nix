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

            wss-client =
              final.haskell.lib.unmarkBroken hprev.wss-client;
          })
        ];
  });
}
