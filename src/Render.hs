-- Render.hs is a program to generate static content for midirus.com
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment (getArgs)

import Audio
import Project
import Toot

main :: IO ()
main =
    getArgs >>= \case
        [] -> mainAll
        ["toot"] -> mainToot
        ["audio"] -> mainAudio
        ["all"] -> mainAll
        _ -> error "unknown command"
  where
    mainAll = do
        mainAudio >> mainProjs >> mainToot
