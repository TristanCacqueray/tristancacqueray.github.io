---
title: Emacs Lisp
tags:
  - pl
  - lisp
---

Emacs Lisp is a dialect of the [[LISP]] programming language family used as a scripting language by [[emacs]].

I wrote a few functions in my [dot-files/emacs.el](https://github.com/TristanCacqueray/dot-files/blob/master/emacs.el).

## Interactive functions

The `git-clone` function does:

- Clone the project to a fully qualified directory (e.g. ~/src/github.com/org/project)
- Open the directory with `dired`

```lisp
(defun giturl-to-dir (url)
  "Convert a git URL to a local path."
  (let ((home (getenv "HOME"))
        (inf (url-generic-parse-url url)))
    (concat
     home "/src/" (url-host inf)
     (replace-regexp-in-string
      "/r/" "/"
      (replace-regexp-in-string
       ".git$" "" (url-filename inf))))))

(defun git-clone-url (url dir)
  (message "clonning %s to %s" url dir)
  (mkdir dir t)
  (call-process
    "git" nil (get-buffer-create "*git-clone-log*") nil "clone" url dir))

(defun f-git? (path)
  (f-directory? (concat path "/.git")))

(defun git-clone (url)
  "Create directory, clone and open project"
  (interactive "Murl: ")
  (let ((d (giturl-to-dir url)))
    (unless (f-git? d)
      (git-clone-url url d))
    (dired d)))
```
