---
title: Emacs Tutorial
tags: [emacs]
date: 2024-10-03
---
<!-- Copyright © 2024 Tristan de Cacqueray -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

There are many tutorials available but this one is mine where
I cover sensible defaults to showcase the standard Emacs experience.

:::{.hidden}
![emacs-tut-corfu](media/emacs-tut-corfu.png)
:::

My goal is to guide you through setting up an IDE similar to VSCode from the ground up.
I will also demonstrate how to use Emacs as a complete integrated computing environment,
allowing you to read news, send emails, play media, and much more!

> [!info]
> This is a work in progress: my plan is to record a screencast that covers the entire content.
> If you encounter any issues, please let me know; I'm committed to keeping this document updated.
>
> This [file][my-emacs-tut] is licensed under a free/libre copyleft license (GPL or CC BY-SA).
>
> **Changelog**
> - 2024-10-05: added the *Development* section.

In this tutorial, I will list all the commands, key bindings, and settings to provide
a comprehensive reference documentation for power users.
This tutorial is based on Emacs version **29**, which is a modern release that works exceptionally well out of the box.

If you are new to Emacs, do not read this in one sitting; take your time and don't panic; you will be here for the long-term.
Feel free to modify the configuration to your liking.
Good luck!


## Context

To put things into perspective, Emacs started in 1976 as a set of macros for the Tape Editor and Corrector (TECO).
GNU Emacs began in 1984 as a true Lisp interpreter, and it is among the oldest free and open-source projects still under
development.

Emacs helps you be productive by offering an integrated environment for a wide range of tasks.
This tool has been tested and proven to work well.

## First contact

In this first session, we will learn the most basic usage.

### Install

Emacs is available on every plateform. Get it with your package manager, for example,
by running `dnf install -y emacs-nox`. This is the lightweight terminal version;
`nox` means no X, which used to be a display server name.
Later, we'll use the full version for multimedias.

### Start

Start Emacs in the terminal like this: `emacs -nw`.
You can ensure a clean start using the `-Q` option.

Emacs keybindings use the <kbd>alt</kbd> and <kbd>ctrl</kbd> modifiers like this:

- `M-x` means press <kbd>alt</kbd>+<kbd>x</kbd>. `M` stands for *meta*, and it is the *alt* key on PC keyboards.
- `C-x 3` means press <kbd>ctrl</kbd>+<kbd>x</kbd>, release <kbd>ctrl</kbd> then press <kbd>3</kbd>.
- `C-h C-q` means press <kbd>ctrl</kbd>+<kbd>h</kbd> then <kbd>ctrl</kbd>+<kbd>q</kbd>. You can also maintain <kbd>ctrl</kbd> while pressing <kbd>h</kbd> then <kbd>q</kbd>.

When you are stuck, or if you want to exit a menu, hit `C-g` repeatedly to stop what Emacs is doing.
If you ever need to quit Emacs, hit `C-x C-c`, but you shouldn't do that :).

After starting Emacs, run the `help-quick` command by pressing `C-h C-q`; your terminal will look like this:

![emacs-tut-quick-help](media/emacs-tut-quick-help.png)

At the bottom, you now have a helpful quick help window that shows you the essential commands.
Once you are comfortable, close the help window by running the same command again.

Here are the only keys you absolutely need to know:

- `M-x`: (<kbd>alt</kbd>+<kbd>x</kbd>) to run a command, and
- `C-g`: (<kbd>ctrl</kbd>+<kbd>g</kbd>) to cancel a command.


### Cursor movements

This section introduces how to move the cursor, which is also called the *point*.

Here are the main cursor movements:

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
| `C-M-f`                               | forward-sexp           | Move forward across one balanced expression.             |
| `C-M-b`                               | backward-sexp          | Move backward across one balanced expression.            |
| `M->`                                 | end-of-buffer          | Move point to the end of the buffer.                     |
| `M-<`                                 | beginning-of-buffer    | Move point to the beginning of the buffer.               |
| `C-v` or <kbd>PageDown</kbd>          | scroll-up-command      | Scroll text of selected window upward.                   |
| `M-v` or <kbd>PageUp</kbd>            | scroll-down-command    | Scroll text of selected window down.                     |
| `C-l`                                 | recenter-top-bottom    | Successive invocation recenter the window.               |
|                                       |                        |                                                          |
| `M-g g`                               | goto-line              | Go to LINE.                                              |
| `M-g c`                               | goto-char              | Go to POSITION.                                          |
| `M-g i`                               | imenu                  | Go to a place chosen using a buffer menu.                |
| `C-x C-@`                             | pop-global-mark        | Go to the last point position.                           |


> Note that these keys mostly work by default with readline (e.g. in bash).

This covers most cursor movements, and with little practice, you will memorize the commands.
Notice how <kbd>ctrl</kbd> is used for short movements while <kbd>alt</kbd> makes longer movements.

> [!tip]
> `C-M-b` (backward-sexp) is not restricted to parenthesis `(`; use it to find matching `[`, `{` or `"` too.

> [!tip]
> Press `C-l` twice to scroll the buffer and move the cursor to the top.
> That's useful to compare two buffers by matching their lines vertically.

Checkout the `M-x help-with-tutorial` to get some practice.

### Window navigation

This section introduces how to manage the window layout.

| *Key*   | *Command*            | *Description*                                        |
|---------|----------------------|------------------------------------------------------|
| `C-x 1` | delete-other-windows | Make WINDOW fill its frame.                          |
| `C-x 2` | split-window-below   | Split WINDOW into two windows, one above the other.  |
| `C-x 3` | split-window-right   | Split WINDOW into two side-by-side windows.          |
| `C-x 0` | delete-window        | Delete WINDOW.                                       |
| `C-x o` | other-window         | Select another window in cyclic ordering of windows. |
| `C-x b` | switch-to-buffer     | Display buffer in the selected window.               |
|         |                      |                                                      |
| `C-x +` | balance-windows      | Balance the sizes of windows shown.                  |

> [!tip]
> Use `balance-window` when you have more than 2 splits.

To move between windows, run `M-x windmove-default-keybindings` to use
<kbd>shift</kbd>+<kbd>arrows</kbd> for moving the cursor to another window:

| *Key*                         | *Command*      | *Description*             |
|-------------------------------|----------------|---------------------------|
| <kbd>shift</kbd>+<kbd>←</kbd> | windmove-left  | Move to the left WINDOW.  |
| <kbd>shift</kbd>+<kbd>↑</kbd> | windmove-up    | Move to the up WINDOW.    |
| <kbd>shift</kbd>+<kbd>↓</kbd> | windmove-down  | Move to the down WINDOW.  |
| <kbd>shift</kbd>+<kbd>→</kbd> | windmove-right | Move to the right WINDOW. |


### File

This section introduces how to open and save a file.

| *Key*     | *Command*      | *Description*                                    |
|-----------|----------------|--------------------------------------------------|
| `C-x C-f` | find-file      | Edit file FILENAME.                              |
| `C-x C-s` | save-buffer    | Save current buffer in visited file if modified. |
| `C-x C-w` | write-file     | Write current buffer into file FILENAME.         |
| `C-x k`   | kill-buffer    | Kill the buffer specified by BUFFER.             |
| `C-x C-q` | read-only-mode | Toggle RO mode.                                  |

If you open a directory instead of a file, Emacs lists the directory contents in a
special buffer named **dired** with the following commands:

| *Key* | *Command*                 | *Description*                              |
|-------|---------------------------|--------------------------------------------|
| `RET` | dired-find-file           | Open the file or directory at point.       |
| `R`   | dired-do-rename           | Rename current file.                       |
| `s`   | dired-sort-toggle-or-edit | Toggle sorting by date.                    |
| `d`   | dired-flag-file-deletion  | Mark a file for deletion.                  |
| `x`   | dired-do-flagged-delete   | Delete the marked files.                   |
| `(`   | dired-hide-details-mode   | Toggle visibility of detailed information. |
| `g`   | revert-buffer             | Refresh the buffer.                        |

> [!tip]
> **Dired** is a powerful file manager, we'll learn more about it later.

### Modes

Every buffer is associated with a mode, which alters certain behaviors, key bindings, and text display in that buffer.
The idea is to customize the appearance and features available based on the contents of the buffer.
The current modes are displayed at the bottom of the buffer in the modeline.
Run the `M-x` command named after the mode to manually toggle its usage.

Emacs has built-in support for many file types, for example, if you open a file named `test.css` and
write `color: pink`, then the buffer will look like this:

:::{.flex .items-center .justify-center}
![emacs-tut-css](media/emacs-tut-css.png)
:::

The mode command lets you toggle the mode, for example you can disable the highlighting with `M-x css-mode`.
You can also remove all modes by activating `M-x fundamental-mode`.

### Edition

Selection happens between a *mark* and the *point*. Here are the main editing keys:

| *Key*          | *Command*          | *Description*                                          |
|----------------|--------------------|--------------------------------------------------------|
| `C-SPC`        | set-mark-command   | Set the mark where point is, and activate it.          |
| `M-w`          | kill-ring-save     | Save ("copy") text between point and mark.             |
| `C-w`          | kill-region        | Kill ("cut") text between point and mark.              |
| `C-y`          | yank               | Reinsert ("paste") the last stretch of killed text.    |
| `C-k`          | kill-line          | Kill the rest of the current line.                     |
| `M-DEL`        | backward-kill-word | Kill previous word.                                    |
| `M-h`          | mark-paragraph     | Put point at beginning of this paragraph, mark at end. |
| `C-x h`        | mark-whole-buffer  | Make region contain the entire buffer ("Select all")   |
|                |                    |                                                        |
| `C-/` or `C-_` | undo               | Undo some previous changes.                            |
| `C-M-_`        | undo-redo          | Undo the last undo.                                    |

> [!tip]
> Use `M-x insert-char` to insert special characters like emojis.

To search for text:

| *Key* | *Command*        | *Description*                   |
|-------|------------------|---------------------------------|
| `C-s` | isearch-forward  | Do incremental search forward.  |
| `C-r` | isearch-backward | Do incremental search backward. |

> [!tip]
> Press the search key again to go to the next match.

To replace text:

| *Key*   | *Command*            | *Description*                                    |
|---------|----------------------|--------------------------------------------------|
| `M-%`   | query-replace        | Replace some occurrences of STRING.              |
| `C-M-%` | query-replace-regexp | Replace some things after point matching REGEXP. |

Once Emacs finds a match, press:

- <kbd>y</kbd> to replace
- <kbd>n</kbd> to skip
- <kbd>!</kbd> to do all replacement without asking.
- <kbd>ctrl</kbd>+<kbd>g</kbd> to cancel.

At this point, your terminal may look like this:

![emacs-tut-dired](media/emacs-tut-dired.png)


## Customization

In this second session we learn how to customize Emacs behaviors and set up some quality-of-life improvements.

### Theme

Setup color theme with `M-x load-theme`:

- `modus-vivendi` for dark.
- `modus-operandi` for light.

These are highly accessible themes by @protesilaos, conforming to the highest standards for colour contrast between background and foreground values (WCAG AAA).

> [!tip]
> If the colors are off, make sure to set the TERM variable, for example start emacs with `TERM=xterm-256color emacs` in tmux

For comparison, Emacs now looks like this:

![emacs-tut-modus](media/emacs-tut-modus.png)


### History

Emacs can remember what you do to help recall past actions:

- `M-x savehist-mode` saves the command history. That way, pressing `M-x` shows the last used commands.
- `M-x save-place-mode` saves the location of the point when you kill a buffer and returns to it the next time you visit the associated file.
- `M-x recentf-mode` saves the recently opened files
- `M-x winner-mode` records the changes in window configuration so that you can revert any layout change with `M-x winner-undo`, for example after running `delete-other-windows` by mistake.

### Lisp Config

Emacs customization is written in the Lisp programing language.
In fact, about 70% of Emacs is written in Lisp, only the bare interface for the operating system is written in C.
Here is a primer on the basic syntax:

- Comments start with: `;;`.
- Call a function (e.g. command) with: `(function-name arg1 arg2)`.
- True and False are `t` and `nil`.
- Symbols are prefixed with `'`, like `'utf-8`.
- List can be written as: `(list 1 2 3)`, or with the quote syntax: `'(1 2 3)`.
- Pairs can be written as: `(cons 42 23)`, or with the dot syntax: `(42 . 23)`.

In the scratch buffer, write:

```scheme
;; Obligatory hello world
(message "Hello %s" 'emacs)
```

Here are useful commands to work with Lisp:

| *Key*     | *Command*       | *Description*                                      |
|-----------|-----------------|----------------------------------------------------|
| `C-x C-e` | eval-last-sexp  | Evaluate sexp before point.                        |
| `M-:`     | eval-expression | Evaluate EXP and print the value in the echo area. |
|           | eval-buffer     | Execute the current buffer as Lisp code.           |


### dot emacs

Add the following to your `~/.emacs` file to ensure the settings persist across restarts:

```scheme
;; Setup a nice theme.
(load-theme 'modus-vivendi)

;; Use shift + arrow keys to change window.
(windmove-default-keybindings)

;; Record window configuration and enable
;; `M-x winner-undo` command to undo layout changes.
(winner-mode)

;; Remember commands and files for easy access.
(savehist-mode)
(save-place-mode)
(recentf-mode)
```

Run `M-x eval-buffer` to apply the settings immediately.

### Settings

Settings are defined as custom variables, check out the `M-x customize` family of
commands to set the options interactively.

To change a setting, use the `setq` command, here are a couple of better defaults:

```scheme
;; Keep auto-save and backup files in one flat directory
;; (instead of next to the original files)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))

;; Store custom variables in ~/.emacs.d/custom.el (instead of ~/.emacs)
(setq custom-file (locate-user-emacs-file "custom.el"))
```

> [!tip]
> Some settings might be overridden after the init,
> for example *indicate-buffer-boundaries* can be reset.
> In that case, try `setq-default` instead.
> *(Let me know if you have a better explanation or how to avoid that quirk)*

When using the interactive `M-x customize-variable` command,
clicking `Save` will store the value into `~/.emacs.d/custom.el`
using this special syntax:

```scheme
(custom-set-variables
 '(require-final-newline t)
 '(load-prefer-newer t)
 '(tab-width 4))
```

To move the customization into your `~/.emacs`, convert the file like this:

```scheme
(setq require-final-newline t)
(setq load-prefer-newer t)
(setq tab-width 4)
```

### Key Bindings

To change a key binding, use the `global-set-key` command, here is a safe change:

```scheme
;; Do not ask for permission to kill a buffer (unless it is modified)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
```
> [!tip]
> Check out the [better-defaults](https://git.sr.ht/~technomancy/better-defaults/) for other safe examples.

### Uniquify

When opening two files with the same name, like `package.json`, it can be confusing to tell their buffers apart.
Use *uniquify* to disambiguate the buffer names, it will rename the buffers with the parent directory when needed:

```scheme
;; Ensure buffer names are unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
```

### package-install

Run `M-x package-install` to install plugins for languages syntax highlighting.
The plugins are usually named *language-mode*, for example run:

- `M-x package-install` <kbd>⏎</kbd> `markdown-mode`

Now, opening a markdown file highlights the syntax.
Here are a few more plugins to get started: `haskell-mode`, `rust-mode` and `yaml-mode`.

### Vert&co

In this section, we set up a completion framework to drastically improve the `M-x` and similar actions.
By default, Emacs comes with `ido`, but we'll use the modern implementation called Vert&co.
Get started with the following commands:

- `M-x package-install` <kbd>⏎</kbd> `vertico`
- `M-x vertico-mode`
- `M-x package-install` <kbd>⏎</kbd> `orderless`
- `M-x eval-expression` <kbd>⏎</kbd> `(setq completion-styles '(orderless basic))`

This enables two key features:

- Prompts show available choices in a vertical buffer.
- Completion is now fuzzy; for example, typing `file find` will select `find-file`.

Here is the result (with the nano-emacs theme):
:::{.flex .items-center .justify-center}
![vertico](../blog/media/vertico-orderless.png)
:::

Then install consult for a better switch-to-buffer command:

- `M-x package-install` <kbd>⏎</kbd> `consult`

Add the following to your `~/.emacs`:

```scheme
;; Setup completion framework
(vertico-mode)
(setq completion-styles '(orderless basic))
(global-set-key (kbd "C-x b") 'consult-buffer)
```

Check out my [[from-ivy-to-vertico]] post for more details.

> [!warning]
> With *vertico*, you might have to use `M-RET` to submit the minibuffer verbatim.
> This can be necessary because `RET` will pick the current selection,
> which prevents creating a new file matching an existing selection,
> for example, to pick `tut.md` when there is a matching `tutorial.md`.

### Bookmarks

Save interesting locations with bookmarks:

| *Command*       | *Description*                                      |
|-----------------|----------------------------------------------------|
| bookmark-set    | Set a bookmark named NAME at the current location. |
| bookmark-jump   | Jump to bookmark BOOKMARK (a point in some file).  |
| bookmark-delete | Delete BOOKMARK-NAME from the bookmark list.       |


> [!tip]
> Bookmarks work with most modes, and as we'll see later, they can
> also be used for remote tramp locations, and eww web pages.
> *consult-buffer* offers bookmark access by default.

---
You now have a fully functional environment.
The next section introduces how to learn on your own before we
dive in more advanced concepts.

## Learn

In this session we learn how to learn within Emacs.


### Describe

Run the following commands to access the documentation and the source of every Emacs
feature:

| *Key*   | *Command*         | *Description*                                                |
|---------|-------------------|--------------------------------------------------------------|
| `C-h f` | describe-function | Display the full documentation of FUNCTION (a symbol).       |
| `C-h v` | describe-variable | Display the full documentation of VARIABLE (a symbol).       |
| `C-h m` | describe-mode     | Display documentation of current major mode and minor modes. |
| `C-h b` | describe-bindings | Display a buffer showing a list of all defined keys.         |
| `C-h k` | describe-key      | Display documentation of the function invoked by KEY-LIST.   |

> [!tip]
> Use `C-h f` to read a command's documentation.

### Which Keys

Install the `which-key` package to get a tooltip about the keys that complete a sequence:

- `M-x package-install` <kbd>⏎</kbd> `which-key`
- `M-x which-key-mode`

Now try pressing `C-h`. After a short delay, the list of all available commands will be
displayed at the bottom.

> [!tip]
> Enable **which-key-mode** to discover available commands.

### Manuals

Read the info pages by running `M-x info`. In the info mode:

| *Key* | *Command*          | *Description*                                                  |
|-------|--------------------|----------------------------------------------------------------|
| `m`   | Info-menu          | The list of current nodes                                      |
| `]`   | Info-forward-node  | Go to the next page.                                           |
| `[`   | Info-backward-node | Go backward one node.                                          |
| `u`   | Info-up            | Go to the superior node of this node.                          |
| `n`   | Info-next          | Go to the "next" node, staying on the same hierarchical level. |
| `t`   | Info-top-node      | Go to the Top node of this file.                               |
| `d`   | Info-directory     | Go to the Info directory node.                                 |
| `M-n` | clone-buffer       | Create and return a twin copy of the current buffer.           |

> [!tip]
> Run `M-x clone-buffer` to persist the buffers that gets auto-replaced, like
> *Help* or *Describe*, that way you can accumulate multiple instances.

To search the info pages, use `M-x consult-info`.

Read the Un*x manual by running `M-x man`.

> [!info]
> The manuals are the ultimate references *(after the source)*. Use them!

## System

In this session we learn how to perform system tasks.

### Process

Run `M-x proced` to list the system processes:

| *Key* | *Command*               | *Description*                                              |
|-------|-------------------------|------------------------------------------------------------|
| `s S` | proced-sort-interactive | Sort Proced buffer.                                        |
| `T`   | proced-toggle-tree      | Toggle the display of the process listing as process tree. |
| `m`   | proced-mark             | Mark a process.                                            |
| `k`   | proced-send-signal      | Send signals to marked processes.                          |
| `f`   | proced-filter           | Filter process.                                            |
| `F`   | proced-format           | Change formatting.                                         |
| `g`   | revert-buffer           | Refresh the buffer.                                        |

Run `M-x list-processes` to list child processes and `M-x list-timers` to list internal tasks.

### Commands

Run standalone shell commands:

| *Key*  | *Command*               | *Description*                                           |
|--------|-------------------------|---------------------------------------------------------|
| `M-!`  | shell-command           | Execute COMMAND in inferior shell                       |
| `M-&`  | async-shell-command     | Execute COMMAND asynchronously in background.           |
| `M-\|` | shell-command-on-region | Execute COMMAND in inferior shell with region as input. |

For example, type `M-!` <kbd>⏎</kbd> `uptime` to display
the age of your host to the minibuffer.
The output is also copied to the **\*Messages\*** buffer.

> [!tip]
> Prefer async commands to avoid locking your session, for example, connect to a local server with:
>
>   `M-&` <kbd>⏎</kbd> `xdg-open http://127.0.0.1:9001`
>
> The input/output will be available in the **\*Async Shell Command\*** buffer.

You can adjust the process environment variables with the `M-x setenv` command.
Note that Emacs uses the `exec-path` variable to discover commands, so you
might want to add your own program locations like this:

```scheme
;; Ensure local scripts are available.
(add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
```

### Shell

Start a shell session with `M-x shell` to create a dumb terminal for running bash.
This works similar to a regular terminal, but you have to press `M-p` to access previous commands.
If needed, see the `vterm` package for a full terminal emulation.

> [!tip]
> Use `C-u M-x shell` to start a secondary shell by using a different buffer name.

There is also `eshell`, which is an advanced mode that lets you run lisp command directly.
Though I don't personally use `eshell`.


## Development

In this session we learn how to write software using Emacs.

### Project.el

Emacs provides a generic framework for managing projects.
This feature is also provided by an external package named `projectile`,
but the core capability is now built in Emacs through `project.el`.

Here are the project commands:

| *Key*     | *Command*                    | *Description*                                         |
|-----------|------------------------------|-------------------------------------------------------|
| `C-x p p` | project-switch-project       | Open a project.                                       |
| `C-x p f` | project-find-file            | Open a file in the current project.                   |
| `C-x p r` | project-query-replace-regexp | Query-replace REGEXP in all the files of the project. |
|           | project-forget-project       | Remove directory PROJECT-ROOT from the project list.  |

### Compile

Emacs comes with a simple yet powerful compilation mode that operates as follows:

| *Command*       | *Description*                                                                    |
|-----------------|----------------------------------------------------------------------------------|
| compile         | starts the build process by selecting a command like `make`.                     |
| project-compile | Run `compile` in the project root.                                               |
| next-error      | jumps to the location of the next error according to `compilation-error-regexp`. |
| recompile       | restarts the compilation process.                                                |

> [!tip]
> The compile workflow is the ideal feedback loop for fixing compilation errors.
> You can also use it with any command that outputs file locations, for example to
> quickly fix a single file.

### Corfu

Corfu enhances in-buffer completion with a small popup.
While an older package named `company` also provides in-buffer completion,
Corfu is a better choice since it integrates seamlessly with the Vert&co ecosystem,
as it is developed by the same community.
For file path completion, we will also utilize the cape package alongside Corfu.

To install Corfu, run the following commands:

- `M-x package-install` <kbd>⏎</kbd> `corfu`
- `M-x package-install` <kbd>⏎</kbd> `corfu-terminal`
- `M-x package-install` <kbd>⏎</kbd> `cape`

Next, add the following to your `~/.emacs`:

```scheme
;; Enable indentation and completion using the TAB key.
(setq tab-always-indent 'complete)

;; Make the completion suggest file paths.
(add-hook 'completion-at-point-functions #'cape-file)

;; Activate in buffer completion everywhere.
(global-corfu-mode)

;; enable corfu in terminal.
;; This is needed until child frame support for terminal Emacs arrives.
(unless (display-graphic-p) (corfu-terminal-mode +1))
```

Here is an example corfu usage to complete a path with orderless
selection:

:::{.flex .items-center .justify-center}
![emacs-tut-corfu](media/emacs-tut-corfu.png)
:::

Once the Corfu menu is open, you can:

- Keep on typing to narrow the selection, and/or
- Use the arrow keys or `M-n` to select an entry.
- Once a single choice is available, pressing <kbd>TAB</kbd> will insert the completion.
- Pressing `C-g` at any time will cancel the menu, as usual.

Completion can occur in two ways: manual or automatic.
I will describe the pros and cons of both solutions,
they are equally valid. However, I find that the manual setup is
easier to get started.

#### Manual completion

Press <kbd>TAB</kbd> to manually trigger the completion.
To use <kbd>SPC</kbd> for separating multiple orderless patterns
(as you do in the minibuffer), you need to configure this key binding:

```scheme
;; For manual corfu, use SPC to add orderless separator.
(keymap-set corfu-map "SPC"  'corfu-insert-separator)
```

> [!tip]
> Some modes might still insert a `TAB` when trying to complete.
> In that case, use `M-TAB`, that should always work.

For reference, the example usage screenshot was produced by typing:\
➡ `"./` <kbd>TAB</kbd> `png` <kbd>SPC</kbd> `logo`

#### Automatic completion

You can make the completion menu appear automatically
after a short delay with:

```scheme
;; show completion automatically after a short delay
(setq corfu-auto t)
```

In this situation, to use orderless with corfu,
we need a way to insert a match separator.
Unlike the minibuffer, `SPC` interupts corfu to prevent misfires.
Therefore, Corfu implements a custom key for in-buffer separator,
which is set to `M-SPC` by default.

For reference, the example usage screenshot was produced by typing:\
➡ `"./` *pause* `png` <kbd>M-SPC</kbd> `logo`.

Unfortunately, `M-SPC` conflicts with the GNOME Shell binding for window menu.
Unless you need this GNOME key, you can disable it by running this
shell command in bash:

```bash
# Disable alt-spc for Corfu... The menu is still available with WIN+right-click.
gsettings set org.gnome.desktop.wm.keybindings activate-window-menu ['']
```

Alternatively, you can bind a different key for the `corfu-insert-separator` command
or simply use the manual completion, which is less problematic overall.

### Prog Mode

Common modes for source code inherit from `prog-mode`, which can be used
to define general behaviors.
For example, you can make the word motions handle camelCase boundaries
by enabling the `subword-mode`:

```scheme
;; This mode changes the definition of a word boundary so that word commands
;; stop at "Gtk" and "Window" in "GtkWindow"
(add-hook 'prog-mode-hook 'subword-mode)
```

Here are some useful prog-mode commands:

| *Key*     | *Command*    | *Description*                      |
|-----------|--------------|------------------------------------|
| `C-x C-;` | comment-line | Comment or uncomment current line. |


### Flymake

Flymake is a minor Emacs mode for performing on-the-fly syntax checks.
It reports errors using squigglies under the offending source code.
Here are the Flymake commands:

| *Command*                        | *Description*                           |
|----------------------------------|-----------------------------------------|
| flymake-mode                     | Toggle Flymake mode on or off.          |
| flymake-goto-next-error          | Jump to the next error.                 |
| flymake-show-buffer-diagnostics  | Show the current buffer's diagnostics.  |
| flymake-show-project-diagnostics | Show the current project's diagnostics. |

Here is an example configuration:

```scheme
;; Activate flymake for every prog-mode buffers.
(add-hook 'prog-mode-hook 'flymake-mode)

;; Use F3 to jump to the next error.
(global-set-key (kbd "<f3>") 'flymake-goto-next-error)
```

> [!tip]
> Run flymake on your `~/.emacs` to see the suggestions.
> We'll address them in the `use-package` section below.

Flymake works standalone using *backend functions* that are usually
provided by file modes. For example *python-mode* uses `pyflakes` by default.
When using the language server client `eglot`, Flymake will use
that as an additional back-end.


### Language server

Emacs comes with a language server client called `eglot`, which functions as follows:

| *Command*                        | *Description*              |
|----------------------------------|----------------------------|
| eglot                            | Start the language server. |
| eglot-shutdown                   | Kill the server.           |
| eglot-code-actions               | Execute server actions.    |
| xref-find-definitions            | Find definition.           |
| xref-find-references             | Find call sites.           |

The current file mode typically configures the relevant server in the `eglot-server-programs` list.
If it's not configured, `eglot` will prompt you for the program or network endpoint to use.

> [!tip]
> The server logs are available in a buffer named \*EGLOT (mode) events\*

Eglot works seamlessly out of the box, requiring no additional configuration or external packages.
It integrates with existing Emacs features like Flymake and Xref.

### Magit

Magit is the user interface for the Git revision control system.

#### Setup

Magit is not available on the default ELPA package registry,
so you need to set up the MELPA registry.
Add the following to your `~/.emacs`:

```scheme
;; Add MELPA registry for Magit.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
```

> After inserting the above snippet, run `M-x eval-buffer`, or simply `C-x C-e` to set the `package-archives` value.

Then run the following commands:

- `M-x package-refresh-contents` <kbd>⏎</kbd>
- `M-x package-install` <kbd>⏎</kbd> `magit` <kbd>⏎</kbd>

Finally, add the following to your `~/.emacs` to bind the main command:

```scheme
;; Quick access to Magit.
(global-set-key (kbd "C-x g") 'magit-status)
```

#### Usage

Inside a repository, run `M-x magit-status` to get the Magit interface.
Here are the main keys:

- <kbd>TAB</kbd>: Expand/Collapse a section.
- <kbd>RET</kbd>: Visit a thing. When the cursor is on a diff,
  this creates a temporary buffers (press <kbd>q</kbd> to close it);
  <kbd>ctrl</kbd>+<kbd>j</kbd> visits the real file.
- <kbd>j</kbd>: Jump to a section.
- <kbd>?</kbd>: View the command list.
- <kbd>$</kbd>: View the actual Git commands that Magit is performing.

To prepare a commit, on a chunk or a file, press:

- <kbd>s</kbd>: Stage
- <kbd>u</kbd>: Unstage
- <kbd>k</kbd>: Trash

Here are the main git commands:

- <kbd>b</kbd>: branch
- <kbd>c</kbd>: commit
- <kbd>P</kbd>: push
- <kbd>f</kbd>: fetch
- <kbd>F</kbd>: pull
- <kbd>l</kbd>: log
- <kbd>M</kbd>: remote
- <kbd>A</kbd>: cherry-pick

> Magit interface doesn't use modifiers like ctrl or alt.

#### Example git commands

After selecting a command, a new buffer appears to let you toggle options.
The argument can be set vertabim, for example by pressing <kbd>-</kbd> followed by the flag, like <kbd>v</kbd>.

Here are some example workflows:

- Push with lease: <kbd>P</kbd> -> <kbd>-</kbd><kbd>f</kbd> -> <kbd>u</kbd>
- Commit amend and reset the author: <kbd>c</kbd> -> <kbd>-</kbd><kbd>R</kbd> -> <kbd>a</kbd>

#### Rewrite history

To start a local rebase, place the cursor on a commit (either in the status page or in the log view) and press <kbd>r</kbd> followed by <kbd>i</kbd>.
A rebase buffer will appear with the list of commits:

- <kbd>alt</kbd>+<kbd>p</kbd>/<kbd>n</kbd>: Reorder a commit.
- <kbd>ctrl</kbd>+<kbd>k</kbd>: Drop a commit.
- <kbd>e</kbd>: Edit a commit.
- <kbd>r</kbd>: Edit a commit message.
- <kbd>s</kbd>/<kbd>f</kbd>: Squash or fixup a commit.

Once you are done, `C-c C-c` to apply the changes, or `C-c C-k` to cancel.

#### Handle conflicts

When there is a conflict, press <kbd>e</kbd> to start an ediff session.
In graphic mode, the control window pops up in a new frame, which is not convenient.
Set the following setting to display the ediff control within your existing frame:

```scheme
;; Show the ediff control window inside the current frame, don't create a new window.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
```

Here are the commands to handle conflicts:

- <kbd>n</kbd>/<kbd>p</kbd>: Go to the next/previous conflict.
- <kbd>a</kbd>/<kbd>b</kbd>: Pick the left or right change. You can also manually fix the edits in the bottom window.
- <kbd>q</kbd>: Quit ediff and apply the resolution.


#### Blame

When viewing a file, run `M-x magit-blame` then press <kbd>b</kbd>:
- <kbd>⏎</kbd>: Open a commit.
- `C-c C-q`: Quit the blame mode (or just re-run `M-x magit-blame` and press <kbd>q</kbd>)


> [!tip]
> Check out the documentation to learn more: <https://magit.vc/>

---
You now have a fully functional integrated developer environment (IDE).
The next section introduces how to write source code in different languages.

## Languages

In this session we learn how to leverage the programing language modes.

### Pretty Print

- `json-pretty-print` and `json-pretty-print-buffer`

### Haskell

- GHCi

### Markdown

Press `TAB` to create tables.

### Rust

rustfmt on save

### Python

- `M-x run-python`
- `M-x python-shell-send-buffer`



## Advanced

In this session, we will learn some advanced features.

### local lisp

- TODO: show how to install git-clone.el.

### server

Enable sub commands like `git commit` or `kubectl edit` to open
a new buffer within your existing session:

```scheme
;; Allow this Emacs process to be a server for client processes.
(server-start)

;; Ensure child-process uses the main Emacs to edit files.
(setenv "EDITOR" "emacsclient")
```

### use-package

use-package lets you maintain a portable configuration.
Until now, we have installed package interactively,
but that's an issue when you copy your config to a fresh environment
because the commands and settings won't be available.

Instead, you need to leverage the `use-package` command to
manage external packages. The basic syntax looks like this:

```scheme
(use-package the-package-name
  :ensure t ;; install if needed
  :config
  (setq the-package-option 'value)
  (global-set-key (kbd "C-x g") 'the-package-command))
```

use-package lets you further declare your config,
for example like this:

```scheme
(use-package the-package-name
  :if (display-graphic-p)
  :diminish
  :custom
  (the-package-option 'value)
  :bind ("C-x g" . the-package-command))
```

… which is equivalent to:

```scheme
(when (display-graphic-p)
  (require 'the-package-name)
  (diminish 'the-package-name)
  (setq the-package-option 'value)
  (global-set-key (kbd "C-x g") 'the-package-command))
```

> [!tip]
> Check out `diminish` to clean up your modeline with use-package,
> and stick to `setq` and `global-set-key` if you prefer.

### stow

Use [stow][stow] to archive your config. Quit emacs now and run the following commands:

[stow]: https://www.gnu.org/software/stow/

```bash
# Move the state to XDG location.
mv ~/.emacs.d ~/.config/emacs

# Setup your stow registry:
mkdir -p ~/src/my-dot-files/home/.config/emacs

# Move the .emacs to init.el
mv ~/.emacs ~/src/my-dot-files/home/.config/emacs/init.el

# Install your home package
stow -v --no-folding -d ~/src/my-dot-files -t ~ home
```

> [!tip]
> Use `--no-folding` to ensure the *emacs* directory stays in your home,
> only the `init.el` will be symlinked.

Keep the general purpose settings in your home stow package,
Then you can configure extra features like emails or feeds in dedicated packages.

### Tab Bar

### Dired

### Multi Cursors

Set up the `multiple-cursors` package to enable editing multiple lines at once:

```scheme
(use-package multiple-cursors
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)))
```

Press <kbd>ctrl</kbd>+<kbd>></kbd> to create multiple cursors. Then edit as usual and complete with <kbd>ctrl</kbd>+<kbd>g</kbd>

### ELisp

- TODO: explain how to write custom commands...


### Templates

Configure custom abbreviations to help you expand repetitive content.
While you can use the builtin `dabbrev` package, here is a more
powerful solution named `tempel`:

```scheme
(use-package tempel
  :bind ("M-*" . tempel-insert)
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))
```

Setup your templates in `~/.config/emacs/templates`:

```scheme
;; ~/.config/emacs/templates
;; https://github.com/minad/tempel#template-syntax

fundamental-mode

(today (format-time-string "%Y-%m-%d"))
(copyr "Copyright © " (format-time-string "%Y ") user-full-name n)

markdown-mode

(kbd "<kbd>" r "</kbd>")
(code "```" r n "```")
```

Here is how to insert templates:

- Type `today` then press `TAB` to insert the current date.
- Select some text, then press `M-*` to apply a template like kbd.


### Embark


## Extra

In this session we will learn extra things you can do with Emacs.

### Micro Features

Emacs contains hundreds of microscopic quality of life features:

- `count-words`
- `sort-lines`
- `fill-paragraph`
- `toggle-truncate-lines`

### ispell

### org mode

### notmuch

### dired

### elfeed

### eww

### erc

### ready-player

### vterm

### tramp

## Next

Here are further resources to continue your journey:

- Guided tour: https://www.gnu.org/software/emacs/tour/index.html
- Beginner resources: https://sachachua.com/web/beginner-map.html
- Light configs: https://quickelisp.dev/ and https://github.com/ebittleman/emacs-bedrock
- Professional config: https://github.com/purcell/emacs.d
- Emacs philosophie: https://protesilaos.com/codelog/2021-09-22-live-stream-emacs-unix/
- Guide for writers: https://github.com/pprevos/emacs-writing-studio
- [The Absolute Beginner’s Guide to Emacs - System Crafters](https://systemcrafters.net/emacs-essentials/absolute-beginners-guide-to-emacs/) - if you prefer videos

[my-emacs-tut]: https://github.com/TristanCacqueray/tristancacqueray.github.io/blob/main/content/emacs/tutorial.md
