---
title: Implementing tiny game engines
date: 2023-02-23
tags:
  - haskell
  - game
  - blog
---

This post presents the techniques I used for the [Haskell Tiny Game Jam][tiny-games-hs].
The goal was to implement a game that fits in 10 lines of 80 characters.
I love this kind of challenge, and inspired by the possibilities, I submitted a few entries for each category.

## Prelude

In this category, no imports are allowed, only the default Prelude from base is provided.
As far as I could tell, only turn-based games are possible because:

- Line buffering can't be disabled; the inputs are only available after pressing the enter key.
- Delay and background rendering are not an option.

For [tiny-brot], I tried to implement a press-forward demo, similar to *trackmania PF*, where
the render loop would be driven by the terminal baud rate.
The user simply keeps pressing enter to render this animation:

:::{.center .mb-2}
![tiny-brot](../static/tiny-brot.gif)
:::

Unfortunately this doesn't look good in practice because the output is produced one character at a time, which causes the cursor to flicker.

For [pure-doors] I used `interact` to implement this game engine:

```haskell
-- engine1.hs
gameLoop :: String -> String
gameLoop (char:'\n':rest) = "Input is: " <> show char <> "!\n" <> gameLoop rest
gameLoop _ = "Done"

main = interact gameLoop
```

It is an interesting solution because it leverages Haskell laziness to evaluate
a pure `String -> String` function. As you can see, the gameLoop does not
call `getChar`, it simply pattern matches the input. This results in
an interactive program:

```ShellSession
$ runhaskell engine01.hs
w
Input is: 'w'!
q
Input is: 'q'!

Done
```

Note that there is nothing special with `interact`, the game loop also runs with:

```haskell
main = putStr . gameLoop =<< getContents
```

Laziness is such an interesting property, in retrospect, I should have called this game *lazy-doors*.

:::{.center .mb-2}
![pure-doors](../static/pure-doors.png)
:::


## Base

In this category, all the base's modules are available. In particular:

- `System.IO.hSetBuffering` enables reading the input one char at a time.

This is great to improve interactivity:

```haskell
-- engine2.hs
import System.IO

eval :: Char -> IO ()
eval char = putStrLn ("Input is: " <> show char)

gameLoop :: IO ()
gameLoop = getChar >>= eval >> gameLoop

main = hSetBuffering stdin NoBuffering >> gameLoop
```

I used this setup for [flower-seeds] so that user can simply press some keys to change the flower.

:::{.center .mb-2}
![flower-seeds](../static/flower-seeds.png)
:::


For [lambda-ray], I used two other base modules to make a smooth animation:

- `Control.Concurrent.threadDelay` to pause between frame.
- `System.Posix.Internals.puts` to print the frame in one syscall and avoid flickering.

```haskell
-- engine3.hs
import Control.Concurrent
import System.Posix.Internals

render :: Int -> String
render t = "Frame: " <> show t

gameLoop :: Int -> IO ()
gameLoop t = puts (render t) >> threadDelay 100_000 >> go (t + 1)

main = gameLoop 0
```

:::{.center .mb-2}
![lambda-ray](../static/lambda-ray.gif)
:::

I was happy to have found `puts`, as it seems like the only[^1] way in base to achieve a smooth animation.

[^1]: There is also `hPutBuf`, but it uses foreign pointers, which is a bit overkill in this context.


## Default

In this category, all the [GHC's default packages][ghc-default] are available.

In particular, `bytestring` can be used to process the user inputs without blocking the rendering loop:

```haskell
-- engine4.hs
import Data.ByteString (ByteString, elemIndices, hGetNonBlocking)
import Control.Concurrent (threadDelay)
import System.IO (stdin, stdout, hSetBuffering, hSetEcho, BufferMode(NoBuffering))

eval :: ByteString -> String
eval input
  | has 119   = "W pressed\n"
  | has 115   = "S pressed\n"
  | otherwise = ""
 where
  has = (/= []) . flip elemIndices input

gameLoop t = do
  threadDelay 100_000
  input <- hGetNonBlocking stdin 42
  putStr (eval input)
  gameLoop (t + 1)

main = hSetBuffering stdin NoBuffering >> hSetEcho stdin False >> gameLoop 0
```

I used this setup for [tiny-space-program][tsp] so that the simulation works as expected:

:::{.center .mb-2}
![tsp](../static/tsp.gif)
:::

A similar implementation can be achieved for the base category using `System.IO.hReady`[^2].
I used `bytestring` because it fits the default category and it exhibits a nice behavior:
when pressing a key, the terminal pauses before repeating the input.
This results in the rocket plume flickering before going full-throttle which provides a little ignition animation for free :)

[^2]: Checkout the base interactive [rhythm entry](https://github.com/haskell-game/tiny-games-hs/tree/main/base/rhythm#readme).

## Terminal Escape Sequence

By printing a special sequence of bytes, we can control the cursor location, color, font styling, and other options on video text terminals.
The bytes are usually produced using a library such as *termcap* or *ncurses*, but it is also possible to hard-code the common [ANSI escape code][ansi-escape-code].
For example:

```haskell
main = do
  -- clear the screen
  putStr "\^[c"
  -- Move the cursor to row 3, column 5
  putStr "\^[[3;5f"
  -- Print in blue
  putStr "\^[[34m"
  putStr "❤ tiny game ❤"
  putStrLn "\n\n"
```

Checkout the [Unicode emoji list](https://unicode.org/emoji/charts/full-emoji-list.html).

## Hackage

In this category, any package available on [Hackage][hackage] can be used. The two most popular options are [ansi-terminal-game] and [gloss].
I used the former for [lazy-march]:

:::{.center .mb-2}
![lazy-march](../static/lazy-march.gif)
:::

The package takes care of the frame rate as well as the terminal escape sequences.


## Conclusion

I really enjoyed this [Haskell Tiny Game Jam][tiny-games-hs].
At first it seemed like an impossible task, but after carefully reading the library's documentation,
I realized that a lot could be done within the 10x80 limit.

Implementing such tiny games took me on a fascinating journey, and it made me appreciate
all the work done on Haskell even more. It's really nice to see that it performs very
well for such a niche use case.

I would like to thank the organizers, the participants and all the fine folks at #[haskell-game](https://matrix.to/#/#haskell-game:matrix.org).

[tiny-games-hs]: https://github.com/haskell-game/tiny-games-hs#readme
[pure-doors]: https://github.com/haskell-game/tiny-games-hs/tree/main/prelude/pure-doors#readme
[tiny-brot]: https://github.com/haskell-game/tiny-games-hs/tree/main/prelude/tiny-brot#readme
[flower-seeds]: https://github.com/haskell-game/tiny-games-hs/tree/main/base/flower-seeds#readme
[lambda-ray]: https://github.com/haskell-game/tiny-games-hs/tree/main/base/lambda-ray#readme
[tsp]: https://github.com/haskell-game/tiny-games-hs/tree/main/default/tsp#readme
[lazy-march]: https://github.com/haskell-game/tiny-games-hs/tree/main/hackage/lazy-march#readme
[ansi-escape-code]: https://en.wikipedia.org/wiki/ANSI_escape_code
[ghc-default]: https://downloads.haskell.org/ghc/latest/docs/libraries/index.html
[ansi-terminal-game]: https://hackage.haskell.org/package/ansi-terminal-game
[gloss]: https://hackage.haskell.org/package/gloss
[hackage]: https://hackage.haskell.org
