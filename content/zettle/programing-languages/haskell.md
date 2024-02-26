---
title: Haskell
tags:
  - pl
  - hm
  - functional/pure
---

John Carmack on Haskell: https://youtu.be/1PhArSujR_A?t=125

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
- Haskell supports Type Level Programing, see: https://vitez.me/hts-language
- [Haskell's kind system - a primer](https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/)
- [Lenses embody Products, Prisms embody Sums](https://blog.jle.im/entry/lenses-products-prisms-sums.html)

- [OCaml for Haskellers](http://blog.ezyang.com/2010/10/ocaml-for-haskellers/)
- [Haskell for OCaml programmers](https://dr-knz.net/haskell-for-ocaml-programmers.html)

## GHC References

To understand the STG, read:

- [Implementing lazy functional languages on stock hardware: the Spineless Tagless G-machine](https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf)
- [Making a Fast Curry: Push/Enter vs. Eval/Apply for Higher-order Languages](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/eval-apply.pdf)
- [STG generated code wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/generated-code)

## Blog

I wrote two articles about the language:

- [Getting Started with Haskell on Fedora](https://fedoramagazine.org/getting-started-with-haskell-on-fedora/)
- [Haskell for python developers](https://www.softwarefactory-project.io/haskell-for-python-developers.html)

## Conf

[[zurihac]]#

[[compositional-type-checking]]#
