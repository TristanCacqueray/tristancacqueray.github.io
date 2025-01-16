{
  inputs = {
    mitadi.url =
      "github:TristanCacqueray/mitadi.nix/6796aa09895d37752e9d4d548502a52e7f8a0d9a";
    # "path:/srv/github.com/TristanCacqueray/mitadi.nix";
    nixpkgs.url =
      "github:NixOS/nixpkgs/3665c429d349fbda46b0651e554cca8434452748";
    nixpkgs-unstable.url =
      "github:NixOS/nixpkgs/de1864217bfa9b5845f465e771e0ecb48b30e02d";
  };

  outputs = inputs:
    let
      emanote = inputs.mitadi.packages.x86_64-linux.default;
      pkgs = import inputs.nixpkgs { system = "x86_64-linux"; };
      ebml = pkgs.fetchFromGitHub {
        owner = "TristanCacqueray";
        repo = "haskell-ebml";
        rev = "aff25512b52e48e92d77cd59019a0291a8b43bf4";
        sha256 = "sha256-U2Mo83gr7dLm+rRKOLzS9LZUaZ90ECO6Zjbv6maflyc=";
      };
      # old ghc used in some article
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
        p.SHA
        (pkgs.haskellPackages.callCabal2nix "ebml" ebml { })
      ]);

      # the new GHC for mitadi
      pkgs-unstable =
        import inputs.nixpkgs-unstable { system = "x86_64-linux"; };
      hspkgs =
        (pkgs-unstable.haskellPackages.extend inputs.mitadi.extend).extend
        (hpPrev: hpFinal: {
          mitadi = hpPrev.callCabal2nix "mitadi" ./src { };
        });
      ghc-unstable = hspkgs.ghcWithPackages
        (p: [ p.emanote p.string-qq p.rio p.lucid p.SHA ]);
      mitadi = hspkgs.mitadi;

      build = pkgs.writeScriptBin "build" ''
        mkdir -p /srv/midirus.com; rm -Rf /srv/midirus.com/*
        cp -p .htaccess robots.txt sitemap.xml /srv/midirus.com
        ${mitadi}/bin/mitadi -L content/ gen /srv/midirus.com
      '';
      run = pkgs.writeScriptBin "run" ''
        ${mitadi}/bin/mitadi -L content/ run --host 0.0.0.0 --port 8080
      '';

      # local-first tools
      run-local-apache = pkgs.writeScriptBin "run" ''
        exec ${pkgs.apacheHttpd}/bin/httpd -f ${local-apache-conf} -DFOREGROUND
      '';

      local-ca = pkgs.runCommand "build-local-ca" { } ''
        echo Generating local ca
        export PATH=$PATH:${pkgs.openssl}/bin
        openssl req -x509 -new -nodes -newkey rsa:2048 -keyout localCA.key -sha256 -days 1825 -out localCA.crt -subj /CN='localhost ca'
        for dns in midirus.com cdn.midirus.com; do
          openssl req -newkey rsa:2048 -nodes -keyout $dns.key -out $dns.csr -subj /CN=$dns -addext subjectAltName=DNS:$dns
          openssl x509 -req -in $dns.csr -copy_extensions copy -CA localCA.crt -CAkey localCA.key -CAcreateserial -out $dns.crt -days 365 -sha256
        done;
        mkdir $out
        mv *.key *.crt $out
      '';

      mk-vhost = name: ''
        <VirtualHost *:80>
            ServerName ${name}
            RewriteEngine On

            <Directory "/srv/${name}">
                AllowOverride All
                Require all granted
            </Directory>

            DocumentRoot /srv/${name}
        </VirtualHost>
        <VirtualHost *:443>
            ServerName ${name}
            RewriteEngine On

            SSLEngine on
            SSLCertificateFile ${local-ca}/${name}.crt
            SSLCertificateKeyFile ${local-ca}/${name}.key

            <Directory "/srv/${name}">
                AllowOverride All
                Require all granted
            </Directory>

            DocumentRoot /srv/${name}
        </VirtualHost>
      '';

      local-apache-conf = pkgs.writeTextFile {
        name = "local-apache.conf";
        text = ''
          ServerRoot "${pkgs.apacheHttpd}"

          # Minimum modules needed
          LoadModule mpm_event_module modules/mod_mpm_event.so
          LoadModule log_config_module modules/mod_log_config.so
          LoadModule mime_module modules/mod_mime.so
          LoadModule filter_module modules/mod_filter.so
          LoadModule deflate_module modules/mod_deflate.so
          LoadModule alias_module modules/mod_alias.so
          LoadModule headers_module modules/mod_headers.so
          LoadModule dir_module modules/mod_dir.so
          LoadModule authz_core_module modules/mod_authz_core.so
          LoadModule unixd_module modules/mod_unixd.so
          LoadModule rewrite_module modules/mod_rewrite.so
          LoadModule ssl_module modules/mod_ssl.so
          TypesConfig conf/mime.types

          PidFile /tmp/httpd.pid

          # Port to Listen on
          Listen *:80
          Listen *:443
          ServerName "local-apache"

          # In a basic setup httpd can only serve files from its document root
          DocumentRoot "/tmp"

          # Default file to serve
          DirectoryIndex index.html

          # Errors go to their own log
          ErrorLog /dev/stderr

          # Access log
          LogFormat "%h %l %u %t \"%r\" %>s %b" common
          CustomLog /dev/stdout common

          # Never change this block
          <Directory />
            AllowOverride None
            Require all denied
          </Directory>

          ${mk-vhost "midirus.com"}
          ${mk-vhost "cdn.midirus.com"}
        '';
      };
    in {
      packages.x86_64-linux.default = mitadi;
      packages.x86_64-linux.local = run-local-apache;
      packages.x86_64-linux.local-ca = local-ca;
      apps."x86_64-linux".default = {
        type = "app";
        program = "${run}/bin/run";
      };
      apps."x86_64-linux".build = {
        type = "app";
        program = "${build}/bin/build";
      };
      devShells."x86_64-linux".mitadi = pkgs.mkShell {
        buildInputs = [
          ghc-unstable
          pkgs-unstable.cabal-install
          pkgs-unstable.ghcid
          pkgs-unstable.haskell-language-server
        ];
      };
      devShells."x86_64-linux".default = hspkgs.shellFor {
        packages = p: [ p.mitadi ];
        buildInputs = [
          pkgs-unstable.cabal-install
          pkgs-unstable.ghcid
          pkgs-unstable.haskell-language-server
          pkgs.flac
          pkgs.id3v2
        ];
      };
      devShells."x86_64-linux".gstreamer = pkgs.mkShell {
        buildInputs = [ ghc pkgs.ghcid pkgs.gst_all_1.gstreamer ];
        GST_PLUGIN_PATH =
          "${pkgs.gst_all_1.gst-plugins-base}/lib/gstreamer-1.0/:${pkgs.gst_all_1.gst-plugins-good}/lib/gstreamer-1.0/";
      };
    };
}
