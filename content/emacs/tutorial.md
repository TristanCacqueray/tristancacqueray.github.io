---
title: Emacs Tutorial
tags: [emacs]
---

There are many tutorials, but this one is mine.

## History

To put things into perspective, Emacs started in 1976 as a set of macros for the Tape Editor and Corrector (TECO).
GNU Emacs began in 1984 as a true Lisp interpreter, and it is among the oldest free and open source projects still under development.

## Start

Start Emacs in the terminal like this: `emacs -nw [file]`.
You can ensure a clean start using the `-Q` option.

Emacs keybindings use the <kbd>alt</kbd> and <kbd>ctrl</kbd> modifiers like this:

- `M-x` means press <kbd>alt</kbd>+<kbd>x</kbd>. `M` stands for *meta*, and it is the *alt* key on PC keyboard.
- `C-x 1` means press <kbd>ctrl</kbd>+<kbd>x</kbd>, release <kbd>ctrl</kbd> then press <kbd>1</kbd>.
- `C-h C-q` means press <kbd>ctrl</kbd>+<kbd>h</kbd> then <kbd>ctrl</kbd>+<kbd>q</kbd>. You can also maintain <kbd>ctrl</kbd> while pressing <kbd>h</kbd> then <kbd>q</kbd>.

When you are stuck, or if you want to exit a menu, hit `C-g` multiple times to stop what Emacs is doing.
If you ever need to quit Emacs, hit `C-x C-c`, but you shouldn't do that :).

After starting Emacs, run the `help-quick` command by pressing `C-h C-q`, your terminal will look like this:

![emacs-tut-quick-help](media/emacs-tut-quick-help.png)

At the bottom you now have a helpful quick help window that shows you the essential commands.
Once you are comfortable, close the help window by running the same command again.

## Cursor movements

In this section I introduce how to move the cursor, also called *point*.

You can use the arrow keys <kbd>←</kbd> <kbd>↑</kbd> <kbd>↓</kbd> <kbd>→</kbd> and <kbd>HOME</kbd> <kbd>END</kbd> <kbd>PageUp</kbd> <kbd>PageDown</kbd>.

Here is how to move the cursor:

| *Key*                                 | *Command*              | *Description*                                            |
|---------------------------------------|------------------------|----------------------------------------------------------|
| `C-a` or <kbd>HOME</kbd>              | move-beginning-of-line | Move point to visible beginning of current logical line. |
| `C-e` or <kbd>END</kbd>               | move-end-of-line       | Move point to end of current line as displayed.          |
| `C-f` or <kbd>→</kbd>                 | forward-char           | Move forward to the next character.                      |
| `M-f` or <kbd>ctrl</kbd>+<kbd>→</kbd> | forward-word           | Move forward to the next word.                           |
| `C-b` or <kbd>←</kbd>                 | backward-char          | Move backward to the previous character.                 |
| `M-b` or <kbd>ctrl</kbd>+<kbd>←</kbd> | backward-word          | Move backward to the previous word.                      |
| `C-p` or <kbd>↑</kbd>                 | previous-line          | Move vertically up                                       |
| `M-{`                                 | backward-paragraph     | Move backward to start of paragraph.                     |
| `C-n` or <kbd>↓</kbd>                 | next-line              | Move vertically down                                     |
| `M-}`                                 | forward-paragraph      | Move forward to end of paragraph.                        |
| `M->`                                 | end-of-buffer          | Move point to the end of the buffer.                     |
| `M-<`                                 | beginning-of-buffer    | Move point to the beginning of the buffer.               |
| `C-v` or <kbd>PageDown</kbd>          | scroll-up-command      | Scroll text of selected window upward.                   |
| `M-v` or <kbd>PageUp</kbd>            | scroll-down-command    | Scroll text of selected window down.                     |

> Note that these keys mostly work by default with readline (e.g. in bash).

That covers 99% of my cursor movement needs, and with a little practice it's easy to get used to.
In particular, notice how <kbd>ctrl</kbd> is used for short movement while <kbd>alt</kbd> makes longer movement.

Checkout the `M-x help-with-tutorial` to get some practice.

## Window navigation

In this section I introduce how to manage the window layout.

| *Key*   | *Command*            | *Description*                                        |
|---------|----------------------|------------------------------------------------------|
| `C-x 1` | delete-other-windows | Make WINDOW fill its frame.                          |
| `C-x 2` | split-window-below   | Split WINDOW into two windows, one above the other.  |
| `C-x 3` | split-window-right   | Split WINDOW into two side-by-side windows.          |
| `C-x 0` | delete-window        | Delete WINDOW.                                       |
| `C-x o` | other-window         | Select another window in cyclic ordering of windows. |
| `C-x b` | switch-to-buffer     | Display buffer in the selected window.               |

To move between windows, run `M-x windmove-default-keybindings` to use <kbd>shift</kbd>+<kbd>arrows</kbd> for moving the cursor to another window.

## File

In this section I introduce how to open and save a file.

| *Key*     | *Command*   | *Description*                                    |
|-----------|-------------|--------------------------------------------------|
| `C-x C-f` | find-file   | Edit file FILENAME.                              |
| `C-x C-s` | save-buffer | Save current buffer in visited file if modified. |
| `C-x k`   | kill-buffer | Kill the buffer specified by BUFFER.             |

## Customization

In this section I introduce how to customize behaviors.

## Configuration

In this section I introduce how to persist configuration to your `~/.emacs.el` file.

```scheme
;; Do not ask for permission to kill a buffer
(global-set-key (kbd "C-x k") 'kill-current-buffer)
```

## Package

In this section I introduce how to install packages.

## Language server

In this section I introduce how to use a language server.

## Magit

In this section I introduce how to git.
