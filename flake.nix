{
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/acb9b59f2dc1cfb11ffe1b1062449c8a5dc2f145";
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;
      haskellExtend = hpFinal: hpPrev:
        let x = 42;
        in {
          slick = pkgs.haskell.lib.dontCheck
            (pkgs.haskell.lib.overrideCabal hpPrev.slick { broken = false; });
        };
      hsPkgs = pkgs.hspkgs.extend haskellExtend;

      ghc = hsPkgs.ghcWithPackages (p: [ p.shake ]);

    in {
      devShell.x86_64-linux =
        pkgs.mkShell { buildInputs = [ ghc pkgs.pandoc ]; };
    };
}
