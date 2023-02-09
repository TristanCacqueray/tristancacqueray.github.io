-- nix develop .#gstreamer --command ghcid ./projects.hs -T main
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson (FromJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS
import Data.List (sortOn)
import Data.String.QQ (s)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Yaml
import RIO
import RIO.Text qualified
import System.Directory (listDirectory)
import System.FilePath

header :: Text
header =
    [s|
---
title: Projects
pandoc:
  rewriteClass:
    plist: grid md:grid-cols-3 gap-4 mb-5
    pcard: rounded border-2 border-blue-100 p-1
    ptitle: text-lg
    pdate: text-sm relative -top-3 -mb-3
---

<!-- note: re-render by running projects.hs -->

Here are some of the projects I have worked on, as an author or contributor.
|]

data Project = Project
    { title :: FilePath
    , pdate :: Text
    , intro :: Text
    }

data ProjectMeta = ProjectMeta
    { date :: Text
    }
    deriving (Generic, FromJSON)

parseProject :: FilePath -> IO Project
parseProject fp = do
    putStrLn $ "[+] " <> fp
    ("---" : lines) <- BS.lines <$> BS.readFile fp
    let (yml, "---" : "" : intro : _) = break (== "---") lines
    ProjectMeta date <- decodeThrow (BS.unlines yml)
    pure $ Project fp date (decodeUtf8With lenientDecode intro)

renderProject :: Project -> Text
renderProject p =
    Text.unlines
        [ ":::{.pcard}"
        , ":::{.ptitle}"
        , "[[" <> Text.pack (takeBaseName $ title p) <> "]]"
        , ":::"
        , ":::{.pdate}"
        , (pdate p)
        , ":::"
        , (intro p)
        , ":::"
        ]

renderProjects :: [Project] -> Text
renderProjects xs =
    Text.unlines
        [ header
        , ":::{.plist}"
        , Text.unlines (map renderProject xs)
        , ":::"
        ]

main :: IO ()
main = do
    projFiles <- map (mappend "projects/") <$> listDirectory "projects"
    projs <- traverse parseProject projFiles
    Text.writeFile "projects.md" (renderProjects (reverse $ sortOn pdate projs))
