-- Render.hs is a program to generate static content for midirus.com
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment (getArgs)

import Audio
import Project
import RIO (exitFailure)
import RIO.Directory (getModificationTime, setModificationTime)
import RIO.FilePath (dropExtension)
import RIO.Prelude (whenM)
import System.Directory (doesFileExist)
import Toot

-- Fixup file timestamps
mainTimestamps :: IO ()
mainTimestamps = do
    all <- runFind ["_out/"]
    epoch <- getModificationTime ".epoch"
    mapM_ (nullTS epoch) all
    files <- runFind ["_out/", "-type", "f"]
    mapM_ setTS files
    putStrLn "Done with timestamps"
  where
    nullTS epoch fp = do
        setModificationTime fp epoch
    setTS fp = do
        ts <- getModificationTime fp
        let outFP = "_out/" <> (drop 8 $ fp)
        doesFileExist outFP >>= \case
            True -> setModificationTime outFP ts
            False -> do
                let baseFP = dropExtension outFP
                whenM (doesFileExist $ baseFP <> ".html") do
                    setModificationTime (baseFP <> ".html") ts
                whenM (doesFileExist (baseFP <> ".xml")) do
                    setModificationTime (baseFP <> ".xml") ts

main :: IO ()
main =
    getArgs >>= \case
        [] -> mainAll
        ["ts"] -> mainTimestamps
        ["toot"] -> mainToot
        ["audio"] -> mainAudio
        ["all"] -> mainAll
        _ -> error "unknown command"
  where
    mainAll = do
        mainAudio >> mainProjs >> mainToot >> mainTimestamps
