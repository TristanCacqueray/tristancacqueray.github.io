---
title: Initiation to the Game of Go
tags:
  - weiqi
---

This document contains instructions to introduce the game, according to [Comment transmettre sa passion aux débutants](https://www.youtube.com/watch?v=4uRUuZNVvqM).
An experienced player can follow this procedure to teach the game to a complete beginner.

## Atarigo

Start by playing atarigo. Give basic instructions:
- Black start, then white, then black, etc...
- Once placed, stones never move,
- But you can capture by surrounding your opponent stones.

Explain how to capture:

```baduk
$$ How many white stones to capture black?
$$ | - - - - - -
$$ | . . . . . .
$$ | . . X X . .
$$ | . . . . . .
```

You can start to play atarigo: first player who capture win the game.

> Games starts with `good game`, and ends with `thanks for the game`.

```baduk
$$c9 Initial atarigo starting stones: the panda shape
$$ | - - - - - - - - - |
$$ | . . . . . . . . . |
$$ | . . . . . . . . . |
$$ | . . . . . . . . . |
$$ | . . . . X O . . . |
$$ | . . . . O X . . . |
$$ | . . . . . . . . . |
$$ | . . . . . . . . . |
$$ | . . . . . . . . . |
$$ | . . . . . . . . . |
$$ | - - - - - - - - - |
```

## Combat

During the games, explain combat techniques:
- Atari
- Double atari
- Shicho
- Death row

For example:
```baduk
$$ Black in a makes atari on white
$$ | - - - - - -
$$ | . . . . . .
$$ | . . . a . .
$$ | . . X O . .
$$ | . . . X . .
```

If the situation doesn't happen, setup the following boards.

### Double atari


```baduk
$$ Black in a creates a double atari
$$ | - - - - - - - - - |
$$ | . . . . . . . . . |
$$ | . . . . . . . . . |
$$ | . . . a O X . . . |
$$ | . . . O X . . . . |
$$ | . . O X . . . . . |
$$ | . . . . . . . . . |
```

> You win atarigo after performing a double atari

### Shicho

```baduk
$$ Black play a or b to threaten a shicho on c
$$ | - - - - - - - - - |
$$ | . . . . . . . . . |
$$ | . . . c X . . . . |
$$ | . . X O O X . . . |
$$ | . . . . X O . . . |
$$ | . . . a b . . . . |
$$ | . . . . . . . . . |
```

### Death row

```baduk
$$ Black capture with a
$$ | - - - - - - - - - |
$$ | . . . . . . . . . |
$$ | . a X . . . . . . |
$$ | . O O X X . . . . |
$$ | . X X O O X . . . |
$$ | . . O O X O . . . |
$$ | . . O X X O . . . |
$$ | . . . . . . . . . |
```

> Tips: show how to place stone with index+middle finger

### Atari

Check understanding of atari:

```baduk
$$ How many stones to capture 1 or 2?
$$ | - - - - - - - - - |
$$ | . . . 1 . . . . X2 |
$$ | . . . . . . . . . |
```

### Shicho explained

Show a defensive move `1` and mimetism with `2`.

```baduk
$$ Black capture with a
$$ | - - - - - - - - - |
$$ | . . . . . . . . . |
$$ | . . . . . . . . . |
$$ | . . . . . 1 . . . |
$$ | . . . a O X . . . |
$$ | . . . . X O . . . |
$$ | . . . . . 2 . . . |
$$ | . . . . . . . . . |
```

> Shicho keeps the oponent at 2 liberties. Mnemonic: white goes out in a direction, black block.

Like double atari, finding a shicho is a winning move in atarigo.

## Territory

In go, the goal is to get territory by dividing the goban.
Play a regular game, using starting stones:

```baduk
$$ Initial 9x9 game
$$ | - - - - - - - - - |
$$ | . . . . . . . . . |
$$ | . . . . . . . . . |
$$ | . . X . O . O . . |
$$ | . . . . . . . . . |
$$ | . . X . . . O . . |
$$ | . . . . . . . . . |
$$ | . . X . X . O . . |
$$ | . . . . . . . . . |
$$ | . . . . . . . . . |
$$ | - - - - - - - - - |
```

- Explain that playing dead stones is forbidden.

- Once the game done, explain that you can pass.

- Show how to arrange the stones to count the score.

- Maybe explain the komi (7.5 points for black).

Play more games by removig starting stones, e.g. without the middle columns.

> Explain the niggiri to decide who starts.

## Immortality

The last technique to learn is immortality. A group needs 2 eyes:

```baduk
$$ Can white play a? Is left dead? Can white capture right?
$$ | - - - - - - - - - |
$$ | . O X a X O . . . |
$$ | . O X X X O . . . |
$$ | . O O O O O . O O |
$$ | O O . . . . O X X |
$$ | X X O . . . O X . |
$$ | . X O . . . O X X |
$$ | . X O . . . O X . |
$$ | X X O . . . O X X |
$$ | O O O . . . O O O |
$$ | - - - - - - - - - |
```

> Immortality requires 2 free distinct intersections.

### Handicap

The teacher might want to try this handicap setup,
Black is likely to win:

```baduk
$$ Jean Paul Gachignard handicaps
$$ | - - - - - - - - - |
$$ | . . . . . . . . . |
$$ | . . . . . . . . . |
$$ | . . . . . . . . . |
$$ | . . . X . X . . . |
$$ | . . . . . . . . . |
$$ | . . . X . X . . . |
$$ | . . . . . . . . . |
$$ | . . . . . . . . . |
$$ | . . . . . . . . . |
$$ | - - - - - - - - - |
```

Try again by removing one stone.

## Ko

Explain the last rules: board position must not be repeated, to avoid infinit capture loop.
At that point, the training is completed.

See [[learn]] to learn more

## Tools

Try these tools to play alone:

- goquest app
- online-go.com
