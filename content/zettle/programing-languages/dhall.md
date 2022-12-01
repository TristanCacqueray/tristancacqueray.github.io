---
title: Dhall
tags:
  - pl
  - functional/pure
---

[Dhall](https://dhall-lang.org/) is a programmable configuration language that you can think of as: JSON + functions + types + imports.
It advertises the absence of Turing-completeness: see this [blog post](http://www.haskellforall.com/2020/01/why-dhall-advertises-absence-of-turing.html).

I made a few contributions to the language, such as converting the github wiki to a sphinx [website](https://docs.dhall-lang.org) or adding
the operator functions to the Prelude. I also worked on adding language support for [wildcard parttern match](https://github.com/dhall-lang/dhall-lang/pull/962).

I created bindings to many configuration format such as:

- [dhall-ansible](https://github.com/softwarefactory-project/dhall-ansible) generated from the SchemaStore json schemas.
- [dhall-containerfile](https://github.com/softwarefactory-project/dhall-containerfile)
- [dhall-nodepool](https://github.com/podenv/podenv/blob/master/docs/discussions/dhall-configuration.md) generated from the voluptuous schemas.
- [dhall-zuul](https://github.com/softwarefactory-project/dhall-zuul)

I wrote an explanation for [[podenv]] [Using dhall for configuration?](https://github.com/podenv/podenv/blob/master/docs/discussions/dhall-configuration.md).
