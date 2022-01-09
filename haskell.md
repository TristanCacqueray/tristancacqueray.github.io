---
title: Haskell
tags:
  - pl
  - hm
  - functional/pure
---

Haskell supports Type Level Programing, see: https://vitez.me/hts-language

## Notes on GHC

Given a program `Foo.hs`:

```haskell
module Foo (bar) where

bar x y = x + y + 42
```

Get the core representation with:

```
$ ghc Foo.hs -ddump-simpl -fmax-simplifier-iterations=0 -dsuppress-uniques -dsuppress-module-prefixes -dsuppress-idinfo
[1 of 1] Compiling Foo              ( Foo.hs, Foo.o )

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 20, types: 17, coercions: 0, joins: 0/0}

-- RHS size: {terms: 13, types: 9, coercions: 0, joins: 0/0}
bar :: forall a. Num a => a -> a -> a
bar
  = \ (@ a) ($dNum :: Num a) (x :: a) (y :: a) ->
      + @ a $dNum (+ @ a $dNum x y) (fromInteger @ a $dNum 42)

-- RHS size: {terms: 5, types: 0, coercions: 0, joins: 0/0}
$trModule :: Module
$trModule = Module (TrNameS "main"#) (TrNameS "Foo"#)
```

## References

- [Why kind-level foralls don't interact with ScopedTypeVariables](https://ryanglscott.github.io/2021/04/05/why-kind-level-foralls-dont-interact-with-scopedtypevariables/) - Great technical writing

## Blog

I wrote two articles about the language:

- [Getting Started with Haskell on Fedora](https://fedoramagazine.org/getting-started-with-haskell-on-fedora/)
- [Haskell for python developers](https://www.softwarefactory-project.io/haskell-for-python-developers.html)

## Conf

<zurihac>
