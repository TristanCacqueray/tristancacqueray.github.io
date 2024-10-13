{
  inputs = {
    emanote.url =
      "github:srid/emanote/48f5ea185873e7b6ee5ab9544a31a8325aea5b61";
    nixpkgs.url =
      "github:NixOS/nixpkgs/3665c429d349fbda46b0651e554cca8434452748";
  };

  outputs = inputs:
    let
      emanote = inputs.emanote.packages.x86_64-linux.default;
      pkgs = import inputs.nixpkgs { system = "x86_64-linux"; };
      ebml = pkgs.fetchFromGitHub {
        owner = "TristanCacqueray";
        repo = "haskell-ebml";
        rev = "aff25512b52e48e92d77cd59019a0291a8b43bf4";
        sha256 = "sha256-U2Mo83gr7dLm+rRKOLzS9LZUaZ90ECO6Zjbv6maflyc=";
      };
      ghc = pkgs.haskellPackages.ghcWithPackages (p: [
        p.markdown-unlit
        p.rio
        p.string-qq
        p.ki
        p.servant
        p.servant-websockets
        p.servant
        p.with-utf8
        p.lucid
        p.servant-lucid
        p.websockets
        p.yaml
        p.pandoc
        p.pandoc-types
        (pkgs.haskellPackages.callCabal2nix "ebml" ebml { })
      ]);
      website = pkgs.stdenv.mkDerivation {
        name = "tristancacqueray.io-pages";
        buildInputs = [ emanote ];
        src = inputs.self;
        # https://github.com/jaspervdj/hakyll/issues/614
        # https://github.com/NixOS/nix/issues/318#issuecomment-52986702
        # https://github.com/MaxDaten/brutal-recipes/blob/source/default.nix#L24
        LOCALE_ARCHIVE =
          pkgs.lib.optionalString (pkgs.buildPlatform.libc == "glibc")
          "${pkgs.glibcLocales}/lib/locale/locale-archive";
        LANG = "en_US.UTF-8";

        buildPhase = ''
          mkdir _out
          emanote -L content/ gen _out
        '';
        installPhase = ''
          mv _out $out
          cp ${inputs.self}/.htaccess $out
        '';
      };
      run = pkgs.writeScriptBin "run" ''
        ${emanote}/bin/emanote -L content/ run --host 0.0.0.0 --port 8080
      '';
    in {
      packages.x86_64-linux.default = website;
      apps."x86_64-linux".default = {
        type = "app";
        program = "${run}/bin/run";
      };
      devShells."x86_64-linux".default =
        pkgs.mkShell { buildInputs = [ ghc pkgs.cabal-install pkgs.ghcid emanote ]; };
      devShells."x86_64-linux".gstreamer = pkgs.mkShell {
        buildInputs = [ ghc pkgs.ghcid pkgs.gst_all_1.gstreamer ];
        GST_PLUGIN_PATH =
          "${pkgs.gst_all_1.gst-plugins-base}/lib/gstreamer-1.0/:${pkgs.gst_all_1.gst-plugins-good}/lib/gstreamer-1.0/";
      };
    };
}
