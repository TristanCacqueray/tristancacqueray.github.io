---
title: 10 Years of Zuul
date: 2022-01-10
tags: [zuul, video]
---

A visualization of the [[zuul|zuul]] development history made with [gource](https://gource.io/).

I composed and produced the music, and made a [[nix]] expression to render the video:

<iframe width="560" height="315" src="https://www.youtube.com/embed/0gLONkPZ1a0" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<br />

Here is the expression:

```nix
# SPDX-License-Identifier: Apache-2.0
#
# Render the video using `$(nix-build ./gource10year.nix)/bin/render.sh`
let
  # The list of project used for the rendering
  repos = [
    (fetchOpendev "zuul" "02efa8fb28af77c63990722f9b21241132a7de60")
    (fetchOpendev "nodepool" "e95b146d10936a9243bdd5639454c3bdd9b62aec")
    (fetchOpendev "zuul-jobs" "90c427d630d2025074448b543b63699c9da13d87")
    (fetchOpendev "zuul-registry" "120cadf2f6a181899e1a9e81c8571edc29a98ece")
    (fetchOpendev "zuul-website" "e7d42af215500c5e2031cd54b456741f9c729d53")
  ];

  # Captions to add highlights
  captions = pkgs.writeTextFile {
    name = "zuul-captions";
    text = ''
      1368158450|Gearman support added (Jenkins multi-master)
      1376413806|Nodepool created
      1379685690|NNFI algorithm added
      1423073263|Cross-repo-dependencies added
      1473959613|Initial jobs launcher
      1522282515|Version 3.0.0 released
    '';
  };

  # Rendering settings
  out-file = "zuul10years.mp4";
  gource-args = [
    "--title 'Zuul History'"
    "--date-format '%Y-%m-%d'" # make the date fixed length
    "--seconds-per-day 0.06" # make the video last <3min
    "--caption-file ${captions}" # add captions
    "-1920x1080" # 1080p video
    "--file-idle-time 10" # reduce clutter
    "--max-file-lag 0.07"
    "--highlight-users"
    "--highlight-dirs"
    "--hide mouse,filenames"
    "--stop-at-end"
    # uncomment to start at a busy time
    # "--start-position 0.8"
  ];
  ffmpeg =
    "${pkgs.ffmpeg}/bin/ffmpeg -y -r 60 -f image2pipe -vcodec ppm -i - -vcodec libx264 -preset ultrafast -crf 1 -threads 0 -bf 0";

  # pin all the dependencies
  nixpkgsPath = fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/a6f258f49fcd1644f08b7b3677da2c5e55713291.tar.gz";
    sha256 = "sha256:0l8cdybgri8jhdmkxr7r1jpnggk6xz4xc5x7ik5v1qn5h2cv6jsz";
  };
  pkgs = (import nixpkgsPath) { };

  # put the repositories in the nix store
  fetchOpendev = name: rev: {
    name = builtins.replaceStrings [ "zuul-" ] [ "" ] name;
    dir = pkgs.runCommand "deep-clone-${name}" {
      GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    } ''
      ${pkgs.git}/bin/git clone https://opendev.org/zuul/${name} $out
      cd $out
      ${pkgs.git}/bin/git checkout -b gource ${rev}
    '';
  };

  # generate repositories history in a linear file
  histories = builtins.concatStringsSep "; " (builtins.map (repo:
    "gource --output-custom-log - ${repo.dir} | sed -E 's#(.+)\\|#\\1|/${repo.name}#'")
    repos);
  mkHistory = pkgs.runCommand "zuul-gource10years-history" { } ''
    export PATH="${pkgs.coreutils}/bin:${pkgs.gnused}/bin:${pkgs.git}/bin:${pkgs.gource}/bin"
    ( ${histories} ) | sort -n > $out
  '';

  args = builtins.concatStringsSep " " gource-args;
  render = pkgs.writeScriptBin "render.sh" ''
    export PATH="${pkgs.coreutils}/bin:${pkgs.git}/bin:${pkgs.gource}/bin:${pkgs.ffmpeg}/bin"
    set -x
    # uncomment to adjust limits when there is more action
    time gource ${mkHistory} ${args} --output-ppm-stream - | ${ffmpeg} ${out-file}
  '';

in render
```
