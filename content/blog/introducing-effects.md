---
title: Introducing an effects system for Monocle
date: 2022-09-27
tags: [blog, haskell, monocle]
---

> This post was initially published on https://www.softwarefactory-project.io/introducing-an-effects-system-for-monocle.html

This blog post explains the reasons we integrated an [effect system][effect-system] in [Monocle][monocle].
This post aims to be beginner friendly.
We understand that some concepts sound intimidating
and we hope that this post demystifies them a bit.

First, it describes the context and its main issue.
Next, it defines key terms and concepts.
Finally, it shows how we used the [effectful][effectful] library to improve Monocle composability.

## Context and problem statement

Monocle components are implemented using dedicated actions.
The goal is to limit the available side-effects and to maintain a clear separation of concerns.

The current implementation is based on the 'ReaderT over IO' pattern and it is affected by a
problem known as the 'n² instances'. The issue is that adding new side-effects requires
unnecessary modifications, which limit the composability and testability of the components.


## What is a side-effect?

A function has [side-effects][side-effect] when it modifies a state outside of its local environment.
Common examples of side-effects include:

- Accessing the filesystem,
- Executing another program, or
- Connecting to a network service.

In other words, a function has side-effects if its output does not depend solely on its input.
It's valuable to identify side-effects because they require extra care when testing and optimizing.

The Haskell type system defines function with side-effects by wrapping the return value in the IO action.
For example, the standard 'readFile' function is defined as `FilePath -> IO String`:
given a file path, 'readFile' returns an IO action that produces a string.

Previously, in Monocle, we used the 'ReaderT over IO' pattern.


## What is a reader?

A reader provides an environment to the functions.
For example, instead of passing the environment using explicit parameters:

```haskell
computeMetric :: Logger -> Database -> Param -> IO Metric
```

A reader can declare the available environment by wrapping the return value:

```haskell
computeMetric :: Param -> ReaderT (Logger, Database) IO Metric
```

This function signature means: given a parameter, 'computeMetric' returns a reader action
that procudes a metric using the `(Logger, Database)` environment.
This lets us focus on the business logic without manually handling the environment.
This is particularly convenient for intermediary functions which don't have to pass
the environment parameters around. In other words, the reader re-arranges the
function's parameters to move the environment out of the way.

A reader is analogous to this Python construct:

```python
class Api:
    def __init__(self, logger, database):
        self.logger = logger
        self.database = database

    def compute_metric(self, param) -> Metric:
        ...
```

This 'Api' object attaches the environment to a general purpose `self` reference which is
passed on to every object method. The `compute_metric` method can freely read and modify the `self` attributes.
On the other hand, the reader action precisely describes the available environment for the `computeMetric` function.

The next sections present how Monocle used to be implemented and what is the benefit of
using an effect system.


## Monocle action contexts

The Monocle component actions were defined as:

- `newtype AppAction a = AppAction (ReaderT AppEnv IO a)` to initialize the index and serve the API.
- `newtype QueryActon a = QueryAction (ReaderT QueryEnv IO a)` to serve user metric.
- `newtype CrawlerAction a = CrawlerAction (ReaderT CrawlerEnv IO a)` to collect changes data.

Instead of using the new types, the individual functions used mtl-style typeclass constraints to enable generic implementations.
For example Monocle had:

- `class TimeContext m`, to enable reading the local time,
- `class RetryContext m`, to catch network error and retry the action with exponential backoff,
- `class LoggerContext m`, to log messages, and
- `class DatabaseContext m`, to access the database.

Such typeclasses are different from Python's class: a typeclass defines a set of methods that is shared across multiple types.
This is analogous to the Rust [trait system][rust-trait].
That means each action needed to provide its own instance, for example:

- `instance DatabaseContext AppAction`
- `instance DatabaseContext QueryAction`

Monocle also defined super constraints for the component code to avoid listing the individual constraint:

- `class (TimeContext m, LoggerContext m, DatabaseContext m) => AppContext m`
- `class (LoggerContext m, DatabaseContext m) => QueryContext m`
- `class (TimeContext m, RetryContext m) => CrawlerContext m`

So that the `computeMetric` function was defined as:

```haskell
computeMetric :: QueryContext m => Param -> m Metric
```

Similarly, the `getChanges` crawler function was defined as:

```haskell
getChanges :: CrawlerContext m => Repository -> m [Changes]
```

Pros:

- Restricted side effects: the function can't do arbitrary IO.
- The constraints can be implemented differently depending on the context.
- The types enforce the available effects. For example, accessing the database from a crawler context is a compile time error.

Cons:

- Adding a new contraint requires adding new instances, the so called 'n² instances' problem.
- This abstraction has an overhead cost, though it was not noticable in Monocle performance.


## Effects system

To improve the Monocle code base, we replaced the mtl-style constraints with an effect system.
Instead of using constraints for the execution context, denoted `m`, Monocle now uses
a list of effect constraints, denoted `es`, along with the `Eff` action provided by the
[effectful][effectful] library.

The main difference is that the effect's environments are defined individually,
and we no longer have to implement the `m` constraint for every context.
Effectful effectively lets us easily compose a list of readers. To learn more about
this technique, checkout the
[Effectful.Dispatch.Static][effectful-dispatch-static] module documentation.

We replaced the super contexts with a type alias to list all the necessary
effects in one place:

- `type QueryEffects es = [LoggerEffect,DatabaseEffect] :>> es`
- `type CrawlerEffects es = [TimeEffect,RetryEffect] :>> es`

And the `computeMetric` and `getChanges` functions are now defined as:

```haskell
computeMetric :: QueryEffects es => Param -> Eff es Metric
getChanges :: CrawlerEffects es => Repository -> Eff es [Changes]
```

The initial refactor aimed for a drop-in replacement so that only the function's signature changed
from `m` to `Eff es`. If you are curious, you can check the [PR 954][pr-mono-effects] which introduced
the new implementation.

Pros:

- This new implementation is arguably simpler: an effect is defined only once.
- Effectful enables seamless integration with the existing Haskell ecosystem.
- Eff is fast: the effect lookup is `O(1)` according to its [documentation](https://hackage.haskell.org/package/effectful-core-2.1.0.0/docs/Effectful-Internal-Effect.html#t:Effect).

Cons:

- The effectful library is relatively new and the ecosystem is still immature.
- The Eff implementation is more complicated than a simple Reader, for example the process known as [unlifting][unlifting-in-2-minutes] requires extra attentions when running concurrently.


## Conclusion

We are satisfied with the transition and we are looking forward to contributing
to the effectful ecosystem by sharing the Monocle implementations.

Please note that behind the 'Action' and 'Context' mentioned in this post,
there is a fundamental structure called a [Monad][monad].
If you are not familiar with the concept already, we recommend this [computerphile video][what-is-a-monad] by Graham Hutton.

Thanks for reading!

[io-tutorial]: https://www.haskell.org/tutorial/io.html
[pr-logging]: https://github.com/change-metrics/monocle/pull/943
[side-effect]: https://en.wikipedia.org/wiki/Side_effect_(computer_science)
[effect-system]: https://en.wikipedia.org/wiki/Effect_system
[effectful]: https://github.com/haskell-effectful/effectful#readme
[pr-mono-effects]: https://github.com/change-metrics/monocle/pull/954
[monad]: https://en.wikipedia.org/wiki/Monad_(functional_programming)
[what-is-a-monad]: https://www.youtube.com/watch?v=t1e8gqXLbsU
[rust-trait]: https://doc.rust-lang.org/book/ch10-02-traits.html
[unlifting-in-2-minutes]: https://github.com/fpco/unliftio#unlifting-in-2-minutes
[effectful-dispatch-static]: https://hackage.haskell.org/package/effectful-core-2.1.0.0/docs/Effectful-Dispatch-Static.html
[monocle]: https://changemetrics.io/
