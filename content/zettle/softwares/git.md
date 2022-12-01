# git

Git is a distributed version-control system for tracking changes in source code during software development.

## Magit

[magit](https://magit.vc/) is an advanced user interface integrated in [[emacs]]

## Snippets

### Create new branch

```
git checkout --orphan pages
```

### Rewrite history using filter-branch

From an existing folder:

```
git clone <git repository A url>
cd <git repository A directory>
git remote rm origin
git filter-branch --subdirectory-filter <directory 1> -- --all
mkdir <directory 1>
mv * <directory 1>
git add .
git commit

git gc --prune=now
```

From a list of files:

```
git ls-files >../r
# Remove entries from the list for files you want to keep.
edit ../r
# Create new history
git filter-branch -f --tree-filter "cat $PWD/../r | xargs rm -rf" --prune-empty HEAD
```

Here is the process I followed for [dhall-openapi](https://github.com/dhall-lang/dhall-haskell/pull/1946).

### Reset submodules

```
git submodule update --init --recursive
```

### Read notes/review branch

```
git fetch gerrit refs/notes/review:refs/notes/review
git log --show-notes=refs/notes/review --no-merges --since 1year | egrep "Code-Review.2" | awk '{ print $2 }' | sort | uniq -c | sort -n
# Compute sum using: awk '{sum+=$1} END {print sum}'
```
