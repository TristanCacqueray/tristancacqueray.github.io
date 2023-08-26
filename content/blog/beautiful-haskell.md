---
title: Beautiful Haskell
date: 2023-08-27
tags: [haskell, blog]
---

Some days there are streams of negative posts about Haskell, with which I disagree.
There are many reasons why I enjoy using Haskell, many of which I have already mentioned
in previous posts. See for example the [Haskell RunTime System](./introducing-butler.md#rts)
or [Why Haxl?](https://github.com/TristanCacqueray/Haxible#why-haxl).
In this post, I present:

- The Haskell 2021 Syntax.
- The Haskell Core language.

## Beautiful Functional Programming

In a discourse post by the same title, Simon Peyton Jones introduced the following [challenge][challenge]:

[challenge]: https://discourse.haskell.org/t/beautiful-functional-programming/7411

### The nested-map-reduce-traversal challenge

This challenge is about transforming a list of lessons nested inside a list of sections
by adding their positions according to the following rules:
- The lessons' position increases globally,
- Until a section resets the counter (indicated by a True value below).

In other words, the goal is to transform such list:

  `[(False, ["a", "b"]), (False, ["c", "d"]), (True, ["e"])]`

into:

  `[[(1, "a"), (2, "b")], [(3, "c"), (4, "d")], [(1, "e")]]`[^2]

[^2]: The lesson "e" is at position 1 because the parent section reset the counter.

Therefor, the challenge is to keep track of the nested counter while traversing
the main list.

### My solution

In this section I introduce the Haskell syntax I used in my solution to the challenge:

```haskell
module Solution where

data Section = Section String Bool [String]

data NumberedSection = NumberedSection Int String [(Int, String)]
    deriving (Show, Eq)

numberSection :: [Section] -> [NumberedSection]
numberSection = go (1, 1)
  where
    go _ [] = []
    go (sectionPos, lessonPos) (Section title reset lessons : rest) =
        numberedSection : go nextPos rest
      where
        numberedSection = NumberedSection sectionPos title numberedLessons
        numberedLessons = zip [lessonStart..] lessons
        lessonStart = if reset then 1 else lessonPos
        nextPos = (sectionPos + 1, lessonStart + length numberedLessons)
```

Here are the key concepts required to read this code:

The first line introduces a `module` which is composed of a
list of declarations.

The first two declarations introduce new types through
`data` declarations, with witch we can use the `deriving`
syntax to generate implementations for formatting (*Show*)
and comparing (*Eq*).

The last declaration is the function *numberSection* that implements the challenge's solution.
The Haskell syntax for declaring a function is: `name argument = value`.
Declarations can have optional type annotations using the `name :: Type` syntax.
Declarations can be nested using the `where` syntax.
Duplicate declarations can be used to pattern match the arguments.
In the above example, *go* is defined twice, once for the base case of
an empty list, and once to unpack the first section.

The Haskell syntax for list can be confusing:
they are created and matched using the `:` operator, or using the standard `[elems..]` syntax.
Finally, the syntax to call a function is `f x y z` (instead of `f(x, y, z)`).

I find the Haskell Syntax really easy to read and write, and I think it is
as simple as it can get.
Of course, Haskell has a lot more to offer.
In the next section I introduce the full syntax I typically use.

### Haskell 2021 Syntax

The Haskell 2021 Syntax is defined by the [haskell 2010 report][haskell2010] along with a bunch of language extensions that are enabled by default through [GHC2021][GHC2021].

[haskell2010]: https://www.haskell.org/onlinereport/haskell2010/haskell.html
[GHC2021]: https://downloads.haskell.org/ghc/9.6.2/docs/users_guide/exts/control.html#extension-GHC2021

When writing Haskell programs, I typically use the following constructs:

- `import` to load declarations from another module.
- `\arg -> value` to introduce anonymous function (e.g. lambda).
- `case` expressions to pattern match with `|` for pattern guards.
- `let` bindings, which are similar to `where`.
- `do` notation to sequence multiple actions and `<-` to bind values.
- `data` records and enums.
- `class` and `instance` for type classes, which are similar to Rust's traits.
- `=>` fat arrow to add type constrains annotation.
- `newtype` to wrap existing types.
- `type` and `pattern` synonyms.
- operators like `>>=`, which are just functions used in infix notation.
- type application with `@`.

None of these constructs are particularly difficult to understand and they complement each other very well.
It's worth noting that Haskell features language extensions
that provide alternative syntax. For example, I
often use the following extensions to make my code looks a bit nicer:
`{-# LANGUAGE OverloadedStrings, BlockArguments, LambdaCase, MultiWayIf, PartialTypeSignatures #-}`.

The syntax may appear complicated on the surface,
but it is in fact built out very simple and practical concepts.
That being said, Haskell also provides advanced features such as `TemplateHaskell`, `GATDs`,
`TypeFamilies` or `LinearType`, however I have not had a chance to use them.

Overall, I think Haskell's syntax can be explained in fewer words than most other languages'.
In the next section I draw a comparison with Python.

### Comparison with Python

Python's built-in syntax contains many elements, for example it has:

- control flow: `while`, `for`, `continue`, `break`, `else`, `pass`, `yield`, `async`, `await`, `return`.
- exceptions: `try`, `except`, `raise`, `assert`.
- data structure: `None`, `dict`, `set` and their operators `and`, `or`, `in`, `is`, `del`, `getattr`, `setattr`.
- objects: `class` inheritance with custom attributes like `__new__`, `__init__`, `__slots__`, `__dict__`, `__eq__`, ...
- function and object `@decorator`.
- scoping keywords with `global`, `nonlocal`, `with`, `as`.

These constructs can be quite subtle, for example, nonlocal is not global.

In total, Python has 35 keywords (see [Lexical analysis - Keywords](https://docs.python.org/3/reference/lexical_analysis.html#keywords)),
while Haskell only has 23 (see [Lexical Structure - Identifiers and Operators](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4)), including 5 keywords I have never used yet (default, foreign, infix, infixl, infixr).
For a fair comparison, we should takes into account the [GHC.Builtin.PrimOps][ghc-primops]
which are exposed with regular function from the [base][base-pkg] library. These are necessary for concurrency and runtime exception handling.
But strictly speaking about the syntax keywords, Haskell is simpler than Python.

[ghc-primops]: https://hackage.haskell.org/package/ghc-9.6.1/docs/GHC-Builtin-PrimOps.html
[base-pkg]: https://hackage.haskell.org/package/base

In the next section, I present the Haskell Core Language, which I think is the key reason
why the Haskell syntax is so simple.

## Haskell Core Language

The Haskell Core Language is an intermediate representation that is used before machine code generation.
Without further ado, here is the [GHC.Core.Expr][core-src] data type[^3]:

[core-src]: https://hackage.haskell.org/package/ghc-9.6.1/docs/GHC-Core.html#t:Expr
[^3]: I believe only 7 of the GHC.Core.Expr constructors are really necessary, tick and coercions where only added later to improve the performance.

```haskell
data Expr b
  = Var   Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]
  | Cast  (Expr b) CoercionR
  | Tick  CoreTickish (Expr b)
  | Type  Type
  | Coercion Coercion
  deriving Data
```

It is possible to convert any Haskell expression into this core representation.
For example, the go definition from above:

```haskell
go [] = []
go (x:rest) = ...
```

… can be rewritten as:

```haskell
go = \arg -> case arg of
  [] -> []
  (x:rest) -> ...
```

… which can be represented using the `Lam` and `Case` core constructors.

It's truly incredible that every Haskell program fits into such a simple shape.
As a matter of fact, I studied this representation as part of my work for the
Haskell Security Response Team (HSRT) where I investigated how to search for vulnerable function calls.
I implemented this simple [getDependenciesFromCore][cabal-audit-core] 40-line function to collect the
dependencies between declarations.

Then I built a program named [cabal-audit][cabal-audit] to traverse the entire call graph of a given Haskell module to
produce such visualizations:

[cabal-audit-core]: https://github.com/TristanCacqueray/cabal-audit/blob/00915f087a5fdfe91a80ad81fa74c0c502498adc/cabal-audit-core/src/CabalAudit/Core.hs#L70-L108
[cabal-audit]: https://github.com/TristanCacqueray/cabal-audit#readme
![cabal-audit-graph](https://user-images.githubusercontent.com/154392/259258034-48874d11-673c-49b0-950c-7e16829aa0b9.jpg)

The Haskell Core language is based on a fundamental concept known as Lambda Calculus,
which is an universal model of computation that can be used to simulate any Turing machine.
This model is so powerful that it has been added to most, if not, all programming languages,
and by using Haskell, you get to use this power from the ground up.


## Conclusion

I am routing for a better language than Haskell.
For example, I hope that Verse will deliver on its promise of
a distributed transactional memory on top of an epic game engine, that sounds fantastic to me.
But until then, Haskell remains my go-to language because it strikes a nice balance between usability, ranging from type level web DSL to hardware description language,
and a magnificent syntax and core foundation.

Of course there are many reasons to avoid Haskell, to name a few:
- The learning curve appears to be never ending,
- Runtime exceptions are difficult to debug,
- There are competing solutions for common problems without an established winner. For example, you can use *cabal* or *stack*, *tasty* or *hspec*, *quickcheck* or *hedgehog*, *streaming* or *conduit*, *attoparsec* or *megaparsec*, *beam* or *opaleye*, *aeson* or *hermes-json*, *fourmolu* or *ormolu*, *blaze* or *lucid*, *mtl* or *effectful*, *lens* or *optics*, *rio* or *relude*, ... This can be paralyzing.

There are also numerous rough edges and paper cuts you have to deal with when using Haskell, but I don't think these nearly outweigh the pleasure of using the language.
