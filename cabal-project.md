# Patching a Haskell build-depends

This post shows some strategies one can use to patch an external library.

## Context

You integrated an external library and it worked well until you realize something is missing, or maybe it is not working as you want.
Here are a few steps you can take to remedy this situation.

As an example, I'll go through my process to produce such [PR][dear-pr] for the [dear-imgui.hs][dear-imgui.hs] project.

---

## Use a local copy

Find the repository URL, for example on hackage, and clone the project locally.
You might want to checkout a release tag and avoid unexpected issues when using the latest version.

In your `cabal.project` file, add:

```
packages:
  -- your project
  .
  -- the library package copy
  /srv/github.com/haskell-game/dear-imgui.hs
```

> If the repository contains multiple packages, then adjust the path to include the right subdir.

Running `cabal run` will rebuild your project using the package copy. At this point, you can make
changes to the local copy, and simply let cabal integrates the modification in your final build.

This is the best strategy to work on patching the affected package.
However this won't work for your users, unless they also have the local copy.
Thus it is time to contribute your change.


## Use a remote copy

If your issue is already fixed upstream, or if you proposed the change, then you can add to your `cabal.project`:

```
source-repository-package
    type: git
    location: https://github.com/TristanCacqueray/dear-imgui.hs
    tag: 6dcac97120b6ff8abacd64f0f75982bd90aed398
    -- if the repository contains multiple packages, set the sub dir:
    -- subdir: foo
```

Running `cabal run` will rebuild the project using the remote copy.
That way, your users are no longer required to setup a local copy.


## The Nix way

When using [nix](https://nixos.org), you can integrate the remote copy like this:

```nix
  haskellExtend = hpFinal: hpPrev: {
    your-project = hpPrev.callCabal2nix "project-name" self { };

    dear-imgui = hpPrev.callCabal2nix "dear-imgui"
      (pkgs.fetchFromGitHub {
        owner = "TristanCacqueray";
        repo = "dear-imgui.hs";
        rev = "6dcac97120b6ff8abacd64f0f75982bd90aed398";
      }) { };
  };
  # final haskell set, see: https://github.com/NixOS/nixpkgs/issues/25887
  hsPkgs = pkgs.haskell.packages.ghc924.extend haskellExtend;
```

Unfortunately this might not be so simple because creating a new nix package may requires extra work.
In that case, it is also possible to apply the change diff directly to the existing nix package:

```nix
  haskellExtend = hpFinal: hpPrev: {
    dear-imgui = pkgs.haskell.lib.overrideCabal hpPrev.dear-imgui {
        patches = [
          (pkgs.fetchpatch {
            url = "https://github.com/TristanCacqueray/dear-imgui.hs/commit/6dcac97120b6ff8abacd64f0f75982bd90aed398.patch";
            sha256 = "sha256-NlSwmfdotXN51ENlPfIe68zM6i2xgeuTy2IS2NI/aqM=";
          })
        ];
      };
  };
```

That's it for today, let me know if you find that post useful.

Cheers!

[dear-pr]: https://github.com/haskell-game/dear-imgui.hs/pull/161
[dear-imgui.hs]: https://github.com/haskell-game/dear-imgui.hs#readme
