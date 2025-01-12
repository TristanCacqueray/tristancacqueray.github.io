-- | The mitadi entry point
module Main (main) where

import Emanote qualified (defaultEmanoteConfig, run)
import Emanote.CLI qualified as CLI
import Emanote.Source.Dynamic (_emanoteCompileTailwind, _emanoteConfigNoteFn)
import Main.Utf8 (withUtf8)
import RIO

import Medias (mainMedia)
import PostFixup (mainPostFixup)
import Project (mainProject)
import System.Environment (getArgs)
import Toot (mainToot)
import WebComponents (addWebComponents)

main :: IO ()
main =
    withUtf8 $
        getArgs >>= \case
            [] -> error "usage: mitadi"
            ["post"] -> mainPostFixup
            ["toot"] -> mainToot
            ["media"] -> mainMedia
            ["all"] -> mainAll
            xs -> mainEma (last xs == "/srv/midirus.com")
  where
    mainEma doPost = do
        cli <- CLI.parseCli
        let config =
                (Emanote.defaultEmanoteConfig cli)
                    { _emanoteCompileTailwind = True
                    , _emanoteConfigNoteFn = addWebComponents
                    }
        Emanote.run config
        when doPost mainPostFixup
    mainAll = do
        mainMedia >> mainProject >> mainToot
