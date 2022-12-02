---
title: Introducing Functional Programming to Pythonistas
date: 2020-12-31
tags: [blog]
---

This year, I continued my journey through computer science by learning functional programming.
I started using functional programming (FP) through emacs lisp, then scheme with the SICP book,
until I discovered Haskell, which is arguably the best language to learn FP concepts.
There I discovered software designs that provide elegant solutions to some of the problems my team and I
are facing.

This post documents why I introduced FP to my team, and what strategies I used.
But first, let's define the FP paradigms.

## Functional programming paradigms

- FP is a programming paradigm where programs are constructed by applying and composing functions.
- FP is declarative programming. Instead of using statements to change the program's state,
  FP expresses the logic of a computation without describing its control flow.
- FP is built on proven general purpose theories such as lambda calculus and abstract algebra.
- FP enables a strong type system. While some FP languages like LISP do not
  feature such type systems, I am refering to FP languages using `System F` which
  enable a powerful and always correct types inference.

To learn more about these paradigms, I recommend Richard Feldman's talk
[The Next Paradigm Shift in Programming][next-paradigm-shift] and Gabriel
Gonzales's post [Why I prefer FP][why-i-prefer-fp].

With that definition out of the way, the next section explains the advantages
of FP paradigms.

## Reasons to use functional programming

As Michael Snoyman explains in his [Economic Argument for Functional Programming][econ-arg-fp] talk,
FP benefits include:

- Reduce code review time.
- Consistent interface and reduced bug count by combining general purpose concepts.
- Remove classes of bugs entirely, reducing testing burden, QA can focus on higher impact bugs.
- Motivate development, leading to better staff retention and easier recruiting.

In particular there are three areas in which I am looking for solutions:

### Development cycle

When we update our software we rely on extensive tests to check the code.
These tests may take hours to complete, resulting in a long feedback loop.
FP may provide a faster feedback loop to improve our productivity.

### Reusability

When we integrate software to build services, we are often dealing with a complex
stack of components that do not share or re-use common computations.
For example, we have many independent TLS setup implementations.
Such components rely on complex state management that is difficult to compose.
FP provides effective tools to define and re-use the common computations.

### System operations

When we operate software, we are often faced with runtime errors
such as AttributeError or ReferenceError. For example, when we restart our CI scheduler,
we may not be able to re-enqueue some changes when they are in an undefined state.
This kind of issue is nasty because:

- The underlying bugs are hard to debug.
- Operating the software in production is stressful.
- Even though we spend a lot of time and energy to prevent those issues, they can still occur.

FP may result in lower bug counts to increase reliability.

However, learning FP has a significant cost of its own.
The next section describes the strategies I used to introduce FP.

## Introducing new concepts

As my team works remotely, I proposed that we set a weekly online event to spend some time together.
Without a specific goal, I meant to create a fun and relaxed space where
we would collaborate outside of our day to day activities. We used a shared remote
desktop environment so that the whole team would see and use the same tools.

This event is not dedicated to FP and each members can propose a subject. For example, we
did a couple of sessions on using GraphQL. We used two different setups for one to
two hours long session:

- Code kata where the presenter would perform a software development exercise,
  explaining each concept along the way.
- Mob programming where the presenter would not drive the session. Instead, each
  participant would discuss and tell the presenter what to do.

Introducing new concepts is more difficult than I anticipated and
I could have spend more time learning pedagogical techniques. In particular, there
is a methodology where the presenter asks the attendees to generate questions ([QFT][qft]) which
seems very effective.
Finding the right question focus and raise interest takes a lot of time, even for professional educators.

The next sections describes each concept I introduced over the year.

## Introducing function composition

### Why

Function composition is a mechanism that combines two functions. It is similar to object
inheritance or object composition, but uses general purpose primitives that can be applied
to a large set of problems. In particular, using function composition results in a more consistent
interface compared to stateful objects.

### How

I shared Scott Wlaschin's talk [The Power of Composition][the-power-of-composition]
and looked for practical use-cases in our code base.
For example, instead of writing this computation:

```python
for build in get_builds():
    if build.status == "NODE_FAILURE":
        continue
    print(build.log_url)
    ...
```

I would suggest using this function composition:

```python
def completed_build(build):
    return build.status != "NODE_FAILURE"

def process(build):
    print(build.log_url)
    ...

list(map(process, filter(completed_build, get_builds())))
```

### Benefits

- Break a computation in smaller pieces.
- Re-use common functions like `map` and `filter`.
- More declarative.

### Results

Thinking in terms of function composition requires a change in habit,
though I think this first concept is easy to introduce. A colleague even
started to use `functools.reduce` when `map` was not enough.

I recommend Julien Danjou's post
[An Introduction to Functional Programming with Python][intro-fp-python]
as a follow-up.

To build up on function composition, I then introduced pure functions.

## Introducing pure function

### Why

For a given input, a pure function always produces the same output.
Thus, it is possible to truly study and understand such functions independently from others
and they provide a solid building block for complex computations.

### How

I introduced Haskell for a small side project where we needed a new service
to forward statsd metrics from UDP packets to two backends: telegraph and prometheus.
We implemented the service in Python ([source][python-udp]) and Haskell ([source][haskell-udp])
in pair programming.

Then I wrote a couple of blog posts about the language: [Getting Started with Haskell on Fedora][haskell-fedora]
and [Haskell for python developpers][haskell-for-python].
I also demonstrated how we could use the language for other use-cases such as documentation management
and software factory packages update.

### Benefits

- Haskell features state-of-the-art programming paradigms.
- Small footprint, the service has been running continuously without any issues.
- More robust, the Haskell network library reports when UDP packets are not
  being delivered, something we were not able to achieve with Python.
- Short development cycle where most of the bugs are prevented by the compiler.

### Results

There is a lot going on in Haskell and there is a steep learning curve.
Learning Haskell is a significant investment and the project I used to introduce the language was too
small to allow for the team to gain adequate experience using the language.
Though new projects are a good opportunity to explore new languages.

## Introducing monadic function

### Why

As Graham Hutton states in the [What is a Monad?][what-is-a-monad] computerphile video,
Monad is one of the most important developments in programming languages in the last twenty-eight years.

### How

I introduced the concept through a code kata using the [Monadic Parser Combinators][monparsing] paper
to implement a RPM spec file parser. I went through the paper by re-implementing each functions in pure Python.
Then I showed how the same concepts can be used for error handling and to manage side effects.

I also presented a couple of Python libraries such as `toolz` and `effect`.

### Benefits

Monad is a general purpose building block that can be applied for many, if not all, computations.
Combined with pure functions, monads enable a clean composition for effectful computations.

### Results

Explaining this concept from the ground up is a difficult exercise and using an existing
combinator library might have been a better strategy. Moreover, Python is not the most ergonomic
language to write purely functional style code, resulting in non idiomatic implementations.

Even though pure monadic functions did not spark much joy for my team,
there is still a lot to learn from FP.
Thus, I switched gears to the more practical type system concept.

## Introducing type system and immutability

### Why

> The fundamental purpose of a type system is to prevent the occurrence of execution
> errors during the running of a program.
>
> -- <cite>Luca Cardelli</cite>

### How

Since python version 3.6, we can add type annotations and use the `mypy` type checker.
So instead of using such object:

```python
def get_build():
    return dict(status="SUCCESS", log_url="http://example")
```

I would suggest to use a `dataclass`:

```python
@dataclass(frozen=True)
class Build:
    status: str
    log_url: str

def get_build() -> Build:
    return Build("SUCCESS", "http://example")
```

I performed a code kata on dataclass and wrote a [blog post][python-dataclass].

### Benefits

- Type checker may prevent runtime errors.
- Re-usable, a library providing dataclass is safer to use.

### Results

This was well received and the benefit of `mypy` is quite practical,
but dataclasses are difficult to introduce in an existing code base.
Thus, we were not able to benefit much from it.

## Introducing strong type system

### Why

A strong type system provides stronger safety and definitely excludes certain classes of programming errors.

### How

I presented algebraic data type to enable type composition in the form of:

- Sum (union)
- Product (record)

For example, the previous build type can also be defined as

```
data build = Error | Success(log_url)
```

For another side project, I presented different languages featuring a strong
type system: PureScript, ELM and ReasonML. We picked ReasonML as the team was
already familiar with React, and I wrote a blog post about this choice:
[Software Factory Resources in Reason][sf-reason].

### Benefits

- Easier to review and maintain.
- Safer dependencies updates, most breaking changes are prevented by the compiler.

### Results

Even though we applied this concept to a web application,
using a strong type system was a very valuable experience.
We could have used Python with some tricks, for example, by enabling the `strict` and
`disallow-any-expr` options. This is impractical because
Python is dearly missing support for tagged union and pattern matching.

To learn more about strong type systems,
I recommend watching this talk by Jane Street: [Why Functional Programming Doesn't Matter][jane-fp-matter].

## Introducing programmable configuration

### Why

Using a general purpose configuration DSL such as Dhall enables a functional approach to
configuration management.

### How

Instead of using a data serialization language like JSON or YAML,
I introduced the Dhall language to define our configurations.

In the `sf-infra` project, where we define our infrastructure, I showed how
by using Dhall records we could define resources such as servers, volumes, and networks.
Then I showed how we could use functions to transform those records into configurations
such as the inventory or the monitoring, and how we can re-use those resources in other
projects.

For the `zuul-operator` I wrote a Dhall function to convert the high level definition of
a Zuul service into a collection of Kubernetes resources and ConfigMap.

### Benefits

- Prevents runtime error, configuration is tested and valid by construction.
- Enables configuration refactoring.
- Fosters re-use through a powerful dependency and import system.

### Results

Adopting a new syntax for configuration is a major difficulty.
Though I showed how this functional language can be used for all our configuration needs,
such as Kubernetes resources, container definitions, CI pipelines and Ansible playbooks.

## Introducing knowledge as code

As presented by Mikael Tönnberg in his [Tech Knowledge-as-Code][tech-knowledge] article,
I re-introduced type systems as a mean to manage software knowledge.

### Why

To capture knowledge in a way that is understandable for both the computer and humans, now and in the future.

### How

Comparing with object's names, documentations and tests, I showed how type annotation
could capture the knowledge more effectively.

In the context of a couple of mob programming sessions, we added types to undocumented code,
resulting in an insightful documentation. Then by increasing the type coverage, we were able
to refine the initial knowledge of the code.

We used the `TypedDict` annotation to incrementally document the `distroinfo` library output.

### Benefits

- Incremental, each modification adds knowledge.
- Mypy verifies the knowledge is correct.

### Results

This concept is relatively easy to demonstrate and it yielded the most engagement from my team.

## Conclusion

Introducing new concepts can be an exhausting process. Most pragmatic engineers are likely to
push back against new ideas, particularly when they induce friction.
As an example, even early languages like Fortran were frowned upon by traditional binary bytecodes authors.
Von Neuman himself was apparently getting [angry][computing-history]
when his students used such language instead of writing the bytecode by hand.

FP vocabulary includes many unfamiliar words like curry, functor or cons. These words refer
to the underlying concept and I think this is great to properly acknowledge what those things are.
However, this causes additional friction and using more familiar words would have
been a better choice to begin with.

Object oriented programming is still one of the most widely used paradigm, and even though it is fairly
[criticized][oop-wikipedia], it is the preferred paradigms in Python. Thus, embracing FP in Python may
not be the best strategy and using a more appropriate language like OCaml or Haskell would be ideal.

At the end of the day, even if FP concepts are not applied directly, they can significantly
improve non-FP development.

[next-paradigm-shift]: https://www.youtube.com/watch?v=6YbK8o9rZfI
[why-i-prefer-fp]: http://www.haskellforall.com/2020/10/why-i-prefer-functional-programming.html
[econ-arg-fp]: https://www.youtube.com/watch?v=n7QETok5hYI
[qft]: https://rightquestion.org/what-is-the-qft/
[the-power-of-composition]: https://www.youtube.com/watch?v=WhEkBCWpDas
[intro-fp-python]: https://julien.danjou.info/python-and-functional-programming/
[python-udp]: https://softwarefactory-project.io/cgit/software-factory/sf-infra/tree/roles/udp-multiplexer/files/udp-multiplexer.py?id=e3eea281571325f1ccb282391613f0035adc121c
[haskell-udp]: https://softwarefactory-project.io/cgit/software-factory/sf-infra/tree/roles/udp-multiplexer/files/udp-multiplexer.hs?id=e3eea281571325f1ccb282391613f0035adc121c
[haskell-fedora]: https://fedoramagazine.org/getting-started-with-haskell-on-fedora/
[haskell-for-python]: https://www.softwarefactory-project.io/haskell-for-python-developers.html
[what-is-a-monad]: https://www.youtube.com/watch?v=t1e8gqXLbsU
[monparsing]: https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf
[python-dataclass]: https://www.softwarefactory-project.io/python-dataclasses.html
[sf-reason]: https://www.softwarefactory-project.io/software-factory-resources-in-reason.html
[jane-fp-matter]: https://www.youtube.com/watch?v=kZ1P8cHN3pY
[tech-knowledge]: https://carboncloud.com/2020/12/07/tech-knowledge-as-code/
[computing-history]: http://www.columbia.edu/cu/computinghistory/index.html
[oop-wikipedia]: https://en.wikipedia.org/wiki/Object-oriented_programming#Criticism
