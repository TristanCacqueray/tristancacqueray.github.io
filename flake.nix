{
  nixConfig.extra-substituters = "https://cache.srid.ca";
  nixConfig.extra-trusted-public-keys =
    "cache.srid.ca:8sQkbPrOIoXktIwI0OucQBXod2e9fDjjoEZWn8OXbdo=";

  inputs = {
    emanote.url =
      "github:srid/emanote/0977bc1dd34d6ebf0eea045b121ed86dce5e2e88";
    nixpkgs.follows = "emanote/nixpkgs";
    flake-parts.url =
      "github:hercules-ci/flake-parts/98bec08c58a9547d705f2f5e300ac8eef6665e52";
    nixpkgs2.url =
      "github:NixOS/nixpkgs/3665c429d349fbda46b0651e554cca8434452748";
  };

  outputs = inputs@{ self, flake-parts, nixpkgs, nixpkgs2, ... }:
    let
      parts = flake-parts.lib.mkFlake { inherit inputs; } {
        systems = nixpkgs.lib.systems.flakeExposed;
        imports = [ inputs.emanote.flakeModule ];
        perSystem = { self', pkgs, system, ... }: {
          emanote.sites."default" = {
            layers = [ ./content ];
            layersString = [ "./content" ];
            port = 8080;
            prettyUrls = true;
          };
        };
      };
      pkgs = import nixpkgs2 { system = "x86_64-linux"; };
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
        p.lucid
        p.servant-lucid
        p.websockets
        (pkgs.haskellPackages.callCabal2nix "ebml" ebml { })
      ]);
      apps = {
        devShells."x86_64-linux".gstreamer = pkgs.mkShell {
          buildInputs = [ ghc pkgs.ghcid pkgs.gst_all_1.gstreamer ];
          GST_PLUGIN_PATH =
            "${pkgs.gst_all_1.gst-plugins-base}/lib/gstreamer-1.0/:${pkgs.gst_all_1.gst-plugins-good}/lib/gstreamer-1.0/";
        };
      };
    in pkgs.lib.foldr pkgs.lib.recursiveUpdate { } [ parts apps ];
}
