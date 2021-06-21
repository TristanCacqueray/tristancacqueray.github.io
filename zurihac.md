# ZuriHac

![](https://raw.githubusercontent.com/zfoh/zfoh.ch/master/images/zurihac2021/static-logo.svg)

[ZuriHac](https://zfoh.ch/zurihac2021/) is the biggest [[haskell]] Hackathon in the world.

Here are some highlights and project I had the opportunity to work on.


## ZuriHac 2020

Talks:

- [Gabriel Gonzalez - “A bare-bones Twitter clone implemented with Haskell + Nix”](https://www.youtube.com/watch?v=Q3qjTVcU9cg) : Use haskell and nix from the ground up.
- [Alexis King - “Effects for Less”](https://www.youtube.com/watch?v=0jI-AlWEwYI) : Introduces how typeclass are resolved and how to improve effect system performance.

Projects:

- [dhall](https://dhall-lang.org) : Worked on [PR 1853](https://github.com/dhall-lang/dhall-haskell/pull/1853) , Gabriel made insightful incremental changes to my implementation.
- [hatrace](https://github.com/nh2/hatrace/) : Added support for syscalls, for example: [PR 82](https://github.com/nh2/hatrace/pull/82).
- [massiv](https://github.com/lehins/massiv) : Attended session on a gaussian stencil implementation.

## ZuriHac 2021

Talks:

- [John Hughes - “Testing smart contracts with QuickCheck“](https://www.youtube.com/watch?v=n4IgYrc0pes).
- [Veronika Romashkina - “Lift Unliftable (and unlift liftable)“](https://www.youtube.com/watch?v=wJsXjsCvSPg) : Introduces monad composition.

Projects:

- [disco](https://disco-lang.readthedocs.io/en/latest/): Worked on low hanging fruits, for example: [PR 272](https://github.com/disco-lang/disco/pull/272).
- [static-haskell-nix](https://github.com/nh2/static-haskell-nix): Helped nh2 build a musl based ghc 8.10, in particular:
  - disabled shpinx for ghc documentation to cutdown the requirements.
  - looked for a working boot ghc.
  - investigated linking error and segfault when building the stage2 ghc
- [karya](https://ofb.net/~elaforge/karya/doc/overview.md.html): Attended hallway presentation of the project.
- [hsbugzilla](https://github.com/juhp/hsbugzilla/): Investigated using Generic to improve the interface [PR 17](https://github.com/juhp/hsbugzilla/pull/17).
- [massiv](https://github.com/lehins/massiv) : Discussed the upcoming v1.0 release.
- [fir](https://gitlab.com/sheaf/fir): Worked on adding a toy example to integrate dear-imgui.hs [MR 19](https://gitlab.com/sheaf/fir/-/merge_requests/19). Then I proposed a plan to improve the toy: [Issue 85](https://gitlab.com/sheaf/fir/-/issues/85)
