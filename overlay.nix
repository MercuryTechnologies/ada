self: super: {
  ada =
    self.haskell.lib.justStaticExecutables
      self.haskellPackages.ada;

  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      self.lib.fold
        self.lib.composeExtensions
        (old.overrides or (_: _: { }))
        [ (self.haskell.lib.packageSourceOverrides {
            ada = ./.;

            base16 = "1.0";
          })
          (hself: hsuper: {
            skews =
              self.haskell.lib.dontCheck
                (self.haskell.lib.unmarkBroken hsuper.skews);

            kdt =
              self.haskell.lib.dontCheck
                (self.haskell.lib.unmarkBroken hsuper.kdt);

            wss-client =
              self.haskell.lib.unmarkBroken hsuper.wss-client;
          })
        ];
  });
}
