# State monad

This post explores the [state monad](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html) in [[haskell]].
Don't forget to checkout the previous post about [[functor-map]].

## Context

Let's consider this counter implementation in [[elixir]]:

```elixir
defmodule MyApp.Counter do
  use GenServer
  def handle_call({:increment, n}, _from, state) do
    state = state + n
    {:reply, state, state}
  end
end
```

This module doesn't own a state, instead it returns a new state on each invocation.
The genserver provides an implementation to pass the state like this:

```elixir
defp receive_loop(state) do
  new_state =
    receive do
      msg -> handle_call(msg, state)
    end
  receive_loop(new_state)
end
```

This model, where the function takes its state as an input, and returns a new state can also be implemented in Python like this:

```python
def increment(state):
    state = state + 1
    return (state, state)

(value, my_counter) = increment(0)
(value, my_counter) = increment(my_counter)
(value, my_counter) = increment(my_counter)
```

Notice how such stateless functions return both a new state, and its output value. We might want to return a different value, for example:

```python
def show_counter(self):
    return ("Counter is at %d" % self, self)

print(show_counter(my_counter))
# Shows ("Counter is at 3", 3)
```

> By eliminating the ability to mutate a state outside of scope, functional programs are often easier to follow and contain fewer bugs.

Let's dive into the State Monad, and how it can be used for this pattern.

## StateT

Start a REPL by running `ghci` (if missing, run `sudo dnf install ghc ghc-mtl-devel`).

```haskell
λ> import Control.Monad.State
λ> :t StateT
StateT :: (s -> m (a, s)) -> StateT s m a
```

The `StateT` has 3 type variables, `s` is the type of the state, `m` is the execution context, and `a` is the return type.
Thus we can define the increment and show_counter like this:

```haskell
λ> type Counter a = StateT Int IO a
λ> increment    = modify (+1) >> get                         :: Counter Int
λ> show_counter = mappend "Counter is at " . show <$> get    :: Counter String
```

And finally we can write the three increments test as:

```haskell
λ> test_counter = increment >> increment >> increment >> show_counter
λ> :t test_counter
test_counter :: Counter String
```

… that we can evaluate like this:

```haskell
λ> :t runStateT
runStateT :: Counter a -> s -> m (a, s)
λ> runStateT test_counter 0
# Shows ("Counter is at 3",3)
```

## StateT step by step

So how does that works? Let's have a closer look step by step.

We first defined a `Counter a` type alias for `StateT Int IO a`.
This type alias indicates that Counter is a computation happening in a `StateT` context, with a state of type `Int` (the counter), running in a `IO` context.
In other words, to obtain the `a` of a `Counter a`, we need to use the `runStateT` to perform the computation.

The benefit is that the `increment` and `show_counter` implementation does not have to deal with the state explicitly, the `StateT` context provides a few helpers:

```haskell
λ> :t modify
modify :: MonadState s m => (s -> s) -> m ()
λ> :t get
get :: MonadState s m => m s
```

And this is possible because `StateT` has an instance of the `MonadState`. This post is not going to try to explain what a Monad is, but here is its definition:

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  pure :: a -> m a
```

Or in other words, a Monad is an Applicative context, which provides the `>>=` operator, also known as `bind` (or `flat_map` or `then`).

Let's break down the `increment` definition.

```haskell
λ> :t modify (+1)
modify (+1)        :: (MonadState Int m) => m ()
λ> :t modify (+1) >> get
modify (+1) >> get :: (MonadState Int m) => m Int
```

This sequencing pattern is so powerful, it has its own syntax, called the `do notation`.
Here is the `increment` function defined using the do notation.

```haskell
increment :: Counter Int
increment = do
  modify (+1) -- increment the counter
  get         -- return the current state
```

> To enter multiline definition in the REPL, first enter `:{`, then paste the code, and finish with `:}`.

Similarly, the `show_counter` can be defined as:

```haskell
show_counter :: Counter String
show_counter = do
  value <- get
  pure $ "Counter is at " <> show value
```

And finally, the `test_counter`:

```haskell
test_counter :: Counter String
test_counter = do
  increment
  increment
  increment
  show_counter
```

This test_counter definition is similar to the previous version we saw. It relies on the `StateT` context to thread the state value in and out of each context.

## Recap

First, we saw how to implement an immutable counter in Python using the same design as Elixir GenServer.
This can be done using a function that takes the current state `s` and return a new state along with the output value `a`.

Then, we observed that this is actually the definition of the haskell's `StateT`: `s -> m (a, s)` (a function that takes a `s`, and return a `(a, s)` inside a context `m`).

And we re-implemented the counter examples using the MonadState which takes care of passing the state around to simplify the implementation down to the essential parts.

## Motivating example

Such abstractions are general purpose, and Monad can be used for other things.
In the previous post we saw the [[functor-map]] and how `traverse` can be used to penetrate nested structures.
Well we can use `traverse` with `StateT`. Let's consider a new function to add a number to our counter:

```haskell
λ> add = modify . (+) :: Int -> Counter ()
```

… which can be used like this

```haskell
λ> flip execStateT 0 $ add 5
5
```

Then we can use traverse to apply the add function to a list of numbers:

```haskell
λ> flip execStateT 0 $ traverse add [5, 11, 11, 15]
42
```

Cheers o/
