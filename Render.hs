-- | Render.hs is a program to generate custom template:
-- - projects list from directory listing
-- - snippets from org mode file
--
-- nix develop --command ghcid ./Render.hs -T main

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson (FromJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS
import Data.List (sortOn)
import Data.String.QQ (s)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Yaml hiding (Parser)
import Lucid
import Lucid.Base (makeAttribute, makeElement, makeElementNoEnd)
import RIO
import RIO.Text qualified
import System.Directory (listDirectory)
import System.FilePath
import Text.Parsec qualified as Parsec
import Text.Parsec.Text (Parser)

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
    picon: relative -top-5 float right-0
---

<!-- note: re-render by running projects.hs -->

Here are some of the projects I have worked on, as an author or contributor.
|]

data Project = Project
    { fp :: FilePath
    , meta :: ProjectMeta
    , intro :: Text
    }

data ProjectMeta = ProjectMeta
    { date :: Text
    , tags :: [Text]
    , title :: Text
    }
    deriving (Generic, FromJSON)

parseProject :: FilePath -> IO Project
parseProject fp = do
    putStrLn $ "[+] " <> fp
    ("---" : lines) <- BS.lines <$> BS.readFile fp
    let (yml, "---" : "" : intro : _) = break (== "---") lines
    pm <- decodeThrow (BS.unlines yml)
    pure $ Project fp pm (parseIntro $! decodeUtf8With lenientDecode intro)

getIcon :: Project -> Maybe (Html ())
getIcon p
    | "fractal" `elem` p.meta.tags = Just "🥦"
    | "design" `elem` p.meta.tags = Just "🎨"
    | "video" `elem` p.meta.tags = Just "🎥"
    | "library" `elem` p.meta.tags = Just "📖"
    | "music" `elem` p.meta.tags = Just "🎵"
    | "extension" `elem` p.meta.tags || "plugin" `elem` p.meta.tags = Just "⚙"
    | "cli" `elem` p.meta.tags = Just cliSvg
    | "web-service" `elem` p.meta.tags = Just srvSvg
    | "nix" `elem` p.meta.tags || "packaging" `elem` p.meta.tags = Just "📦"
    | "contributor" `elem` p.meta.tags = Just "🧑"
    | "game" `elem` p.meta.tags = Just "🎮"
    | otherwise = Nothing

viewBox_ = makeAttribute "viewBox"
fill_ = makeAttribute "fill"
d_ = makeAttribute "d"
path_ = makeElement "path"

srvSvg =
    with svg_ [xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 24 24", width_ "24", height_ "24"] do
        with path_ [fill_ "none", d_ "M0 0h24v24H0z"] mempty
        with path_ [d_ "M5 11h14V5H5v6zm16-7v16a1 1 0 0 1-1 1H4a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h16a1 1 0 0 1 1 1zm-2 9H5v6h14v-6zM7 15h3v2H7v-2zm0-8h3v2H7V7z"] mempty

cliSvg = with svg_ [xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 24 24", width_ "24", height_ "24"] do
    with path_ [fill_ "none", d_ "M0 0h24v24H0z"] mempty
    with path_ [d_ "M3 3h18a1 1 0 0 1 1 1v16a1 1 0 0 1-1 1H3a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1zm9 12v2h6v-2h-6zm-3.586-3l-2.828 2.828L7 16.243 11.243 12 7 7.757 5.586 9.172 8.414 12z"] mempty

renderProject :: Project -> Html ()
renderProject p =
    with div_ [class_ "rounded border-2 border-blue-100 p-1"] do
        with div_ [class_ "text-lg"] do
            let t = Text.pack (takeBaseName $ fp p)
            with a_ [class_ "text-blue-600 mavenLinkBold hover:underline", href_ ("project/" <> t)] do
                toHtml (if (Text.length p.meta.title < Text.length t) then p.meta.title else t)

            case getIcon p of
                Just ico -> with span_ [class_ "float-right"] ico
                Nothing -> pure ()

        with span_ [class_ "text-sm relative -top-1"] (toHtml (p.meta.date))
        div_ (toHtml (intro p))

renderProjects :: [Project] -> Html ()
renderProjects xs = with div_ [class_ "grid md:grid-cols-3 gap-4 mb-5"] do
    traverse_ renderProject xs

parseIntro :: Text -> Text
parseIntro t = case Parsec.runParser introParser () "" t of
    Left e -> error (show t <> " -> " <> show e)
    Right xs -> (mconcat . takeWhile (/= ".") $ xs) <> "."

introParser :: Parser [Text]
introParser = Parsec.many1 (wordP <|> linkP <|> dotP)
  where
    dotP = "." <$ Parsec.char '.'
    wordP = Text.pack <$> Parsec.many1 (Parsec.satisfy (`notElem` ['.', '#', '[', ']']))
    linkP = do
        Parsec.optional (Parsec.char '#')
        Parsec.many1 (Parsec.char '[')
        l <- Text.pack <$> Parsec.many1 (Parsec.satisfy (`notElem` [']']))
        Parsec.many1 (Parsec.char ']')
        Parsec.optional $ do
            Parsec.char '('
            Parsec.many1 (Parsec.satisfy (`notElem` [')']))
            Parsec.char ')'
        Parsec.optional (Parsec.char '#')
        pure $ if Text.elem '|' l then Text.takeWhileEnd (/= '|') l else l

formatIntro :: [Text] -> Text
formatIntro = flip mappend "." . mconcat . takeWhile (/= ".")

main :: IO ()
main = do
    projFiles <- map (mappend "content/project/") <$> listDirectory "content/project"
    projs <- traverse parseProject projFiles
    renderToFile "content/templates/components/projects.tpl" (renderProjects (reverse $ sortOn (date . meta) projs))
