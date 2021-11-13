# Functor map

This post explores the map function in [[purescript]].
Start a REPL by running these commands in a new directory:

```
mkdir test-functor; cd test-functor
spago init
spago install maybe arrays node-process
spago repl
```

## Context

Let's consider this data type to represent a task:

```haskell
> data Task = Task { name :: String, mem :: Int, cpu :: Int }
```

And this function to create a new task:

```haskell
> mkTask name = Task { name, mem: 0, cpu: 0 }
> :t mkTask
String -> Task
```

(And copy paste this show instance to see the result in the REPL):

```haskell
instance showTask :: Show Task where show (Task t) = "(Task " <> t.name <> ")"
```

We can create a task:

```haskell
> task = mkTask "worker"
> task
(Task worker)

> :t task
Task
```

So far so good, nothing special to see here.


## Mapping mkTask

Here is the definition of `map mkTask`:

```haskell
> :t mkTask
               String ->   Task
> :t map mkTask
Functor f => f String -> f Task
```

`map mkTask` is a function that takes a Functor holding a String, and it returns the same Functor holding a Task.
In otherwords, `map mkTask` inject the `mkTask` function inside the Functor.
In other otherwords, `map` embellished `mkTask` to work with Functors.

Here are some example usages, using the Maybe Functor (don't forget to `import Data.Maybe`):

```haskell
> :t map mkTask Nothing
Maybe Task

> map mkTask Nothing
Nothing

> map mkTask (Just "worker")
(Just (Task worker))
```

Or with the Array Functor:

```haskell
> :t map mkTask []
Array Task

> map mkTask ["worker1", "worker2"]
[(Task worker1), (Task worker2)]
```

> Thanks to the Functor abstraction, we are able to modify the value contained in different structure using a common map function.

## Map definition

map is defined as follow:

```haskell
> :t map
forall f a b. Functor f => (a -> b) -> f a -> f b
```

There are three parts (separated by `.` and `=>`):

- `forall f a b` is the quantification. That means there will be three type variables used in the definition: `f`, `a`, and `b`. For more details read [wasp-lang/haskell-handbook/forall.md](https://github.com/wasp-lang/haskell-handbook/blob/master/forall.md).
- `Functor f` is a constraint. That means the `f` type variable needs to be a Functor. For more details read [pursuit Functor](https://pursuit.purescript.org/packages/purescript-prelude/5.0.0/docs/Data.Functor#t:Functor).
- `(a -> b) -> f a -> f b` is the function signature. That means this function expects two arguments, `a -> b` and `f a`, and it returns a `f b`.

Here `f` is a type constructor, in the signature it is given a type. That means `f` expects a type argument to become a final type. For more details read [purescript-book/chapter3](https://book.purescript.org/chapter3.html#type-constructors-and-kinds), or watch this [An introduction to Haskell's kinds](https://www.youtube.com/watch?v=JleVecHAad4) video by Richard A. Eisenberg.

For example `Maybe` is a type constructor, you can't use it directly, it needs an extra type:

```haskell
> :k Maybe
Type -> Type

> :k Maybe String
Type
```

> You can check that Maybe is indeed a functor by looking up its instance list: [pursuit Maybe](https://pursuit.purescript.org/packages/purescript-maybe/5.0.0/docs/Data.Maybe#t:Maybe).

Now Let's see why this works.

## Map type variables

The map definition is polymorphic, that means it can work in many scenario depending on its arguments.
We can observe how the type checker works by providing the argument one by one:

```haskell
> :t map
forall f a b. Functor f => (a -> b) -> f a      -> f b

> :t map mkTask
forall f.     Functor f =>             f String -> f Task

> :t map mkTask []
                                                   Array Task
```

Notice how when using `mkTask` the type variable `a` becomes a String, and the `b` becomes a Task. This is because these types are no longer variable after we use `mkTask`: the polymorphic argument `a -> b` becomes `String -> Task`, and the other variable name occurences are replaced accordingly (from `f a -> f b` to `f String -> f Task`)


We can also change the order of the argument to provide the functor before the function using `flip`:

```haskell
> :t flip map
forall f a b. Functor f => f a -> (a      -> b) -> f b

> :t flip map []
forall b.                         (a      -> b) -> Array b

> :t flip map ["x"]
forall b.                         (String -> b) -> Array b

> :t flip map ["x"] mkTask
                                                   Array Task
```

`["x"]` being a `Array String`, notice how when using it the type variable `f` becomes an Array, and the `a` becomes a String. This is because `["x"]` is used for the argument `f a` which then becomes `Array String`, and thus the other affected variable name occurences are replaced accordingly (from `(a -> b) -> f b` to `(String -> b) -> Array b`)

> This process can be refered to as specialization, and it is helpful to understand function signature by removing type variables.

## Motivating example

Finally, here is a last example to demonstrate map with `lookupEnv`.

```haskell
> import Node.Process
> :t lookupEnv
String -> Effect (Maybe String)
```

`lookupEnv` expects a name, and it returns an `Effect` containing an optional value.

```haskell
> lookupEnv "USER"
(Just "tdecacqu")
```

> Note that the REPL automatically perform the `Effect`.

We already saw how we can change the value of a Maybe using map, but `lookupEnv` returns an extra Effect layer. Let's consider this double map usage:

```haskell
> :t map (map mkTask)
forall f g. Functor f => Functor g => f (g String) -> f (g Task)
```

We can use it to penetrate both the Effect and the Maybe functor to modify the final value in one shot while preserving the structure:

```haskell
> :t map (map mkTask) (lookupEnv "USER")
Effect (Maybe Task)

> map (map mkTask) (lookupEnv "USER")
(Just (Task tdecacqu))
```

Map is so powerful it has an operator version: `<$>` which let us rewrite this code as:

```haskell
> map mkTask <$> lookupEnv "USER"
```

Which means, given the `Effect (Maybe String)` returned by lookupEnv, we'll inject `map mkTask` into the `Effect`, to convert the optional value into an optional Task.

And we can use this for any two functors combinaison, for example, a list of optional string:

```haskell
> xs = [Nothing, Just "worker1", Just "worker2"] :: Array (Maybe String)
> map mkTask <$> xs
[Nothing, (Just (Task worker1)), (Just (Task worker2))]
```

And this concludes the exploration. Thanks for your time!


## Bonus: traverse

Well while you are here, here is `traverse`:

```haskell
> import Data.Traversable
> :t traverse
forall t m a b. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
```

Nothing special here, we know how to read this. To recap, this definition means:

- There are 4 type variables: `t`, `m`, `a` and `b`.
- `t` is a Traversable, which is a foldable functor (and most Functor are Traversable), see this for more details: [pursuit Traversable](https://pursuit.purescript.org/packages/purescript-foldable-traversable/5.0.1/docs/Data.Traversable#t:Traversable).
- `m` is an Applicative, and let's not bother with what that means exactly, but just know that `Effect` is an applicative: see its instance list: [pursuit Effect](https://pursuit.purescript.org/packages/purescript-effect/3.0.0/docs/Effect#t:Effect).

Thus, given a function `a -> m b`, and a traversable `t a`, traverse will produce a `m (t b)`.

For example we can use traverse with lookupEnv because lookupEnv is compatible with `a -> m b`, it is `String -> Effect (Maybe String)`:

```haskell
> :t traverse lookupEnv
forall t. Traversable t => t String -> Effect (t (Maybe String))
```

Notice how the lookupEnv definition sets the type variable `a` to String, `m` to Effect and `b` to (Maybe String), leaving us with the last type variable `t`.

This definition means that given a collection of string, `traverse lookupEnv` will perform each individual lookup and return the result wrapped in a single Effect:

```haskell
> :t traverse lookupEnv ["USER", "HOSTNAME"]
Effect (Array (Maybe String))

> traverse lookupEnv ["USER", "HOSTNAME"]
[(Just "tdecacqu"), (Just "localhost")]
```

This result uses 3 Functors: Effect, Array and Maybe. And of course we can use maps to penetrate all the layers:

```haskell
> :t map (map mkTask) <$> traverse lookupEnv ["USER", "HOSTNAME"]
Effect (Array (Maybe Task))

> map (map mkTask) <$> traverse lookupEnv ["USER", "HOSTNAME", "OOPS"]
[(Just (Task tdecacqu)), (Just (Task localhost)), Nothing]
```

Or using function composition:

```haskell
> traverse (map (map mkTask) <<< lookupEnv) ["USER", "HOSTNAME"]
[(Just (Task tdecacqu)), (Just (Task localhost))]
```

Which is really convenient as we don't have to unwrap anything.
Using map we modify `lookupEnv` to convert a traversable structure of `String` into `Task`s by reading their value from the environment:

```haskell
> :t traverse (map (map mkTask) <<< lookupEnv)
forall t. Traversable t => t String -> Effect (t (Maybe Task))
```

Cheers o/
