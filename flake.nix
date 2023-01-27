{
  nixConfig.extra-substituters = "https://cache.garnix.io";
  nixConfig.extra-trusted-public-keys =
    "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=";

  inputs = {
    emanote.url =
      "github:EmaApps/emanote/24ca8d95ac2aec8a02a128382081ac076451d018";
    nixpkgs.follows = "emanote/nixpkgs";
    flake-parts.follows = "emanote/flake-parts";
    nixpkgs2.url =
      "github:NixOS/nixpkgs/3665c429d349fbda46b0651e554cca8434452748";
  };

  outputs = inputs@{ self, flake-parts, nixpkgs, nixpkgs2, ... }:
    let
      parts = flake-parts.lib.mkFlake { inherit self; } {
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
        rev = "aba7eb804330f2e42009aa7abb68a8a7da704ee5";
        sha256 = "sha256-l2GxjT4WTX6P+OhctMP9qGWZkP5hBZ+Ba21TVa/4ps8=";
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
