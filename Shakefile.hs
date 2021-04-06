#!/bin/env nix-shell
#! nix-shell -p pandoc -p "haskellPackages.ghcWithPackages (p: [p.shake] )" -i runghc

-- |
module Shakefile where

import Development.Shake
import Development.Shake.FilePath (splitDirectories)

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = "_build"} $ do
  want ["worktree", "pages", "talks"]

  phony "worktree" $ do
    putInfo "TODO: setup worktree..."

  phony "pages" $ do
    need ["worktree"]
    putInfo "pagings..."
    cmd_ "neuron rib -o output/"

  phony "talks" $ do
    need ["worktree"]
    cmd_ "mkdir -p output/talks"
    cmd_ "rsync talks/style.css output/talks/style.css"
    talk "talks/dhall-strong-alternative-to-yaml/"
  where
    pandoc = "pandoc -t slidy --css ../style.css --highlight-style pygments -s"
    talk :: FilePath -> Action ()
    talk dir = do
      let odir = "output/" <> dir
      cmd_ "rsync -a " dir odir
      cmd_ pandoc (odir <> (last (splitDirectories dir)) <> ".md") "-o" (odir <> "index.html")
