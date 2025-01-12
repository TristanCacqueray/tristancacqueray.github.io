{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid reverse" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use forM_" #-}

-- | Render the project.tpl
module Project (mainProject) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS
import Data.List (sortOn)
import Data.Text qualified as Text
import Data.Yaml hiding (Parser, encodeFile)
import Lucid
import Lucid.Base (makeAttribute, makeElement)
import RIO
import System.Directory (listDirectory)
import System.FilePath
import Text.Parsec qualified as Parsec
import Text.Parsec.Text (Parser)

import System.Process.Typed qualified as Process

data Project = Project
    { fp :: FilePath
    , meta :: ProjectMeta
    , intro :: Text
    }

projectMeta :: Project -> ProjectMeta
projectMeta proj = proj.meta

data ProjectMeta = ProjectMeta
    { date :: Text
    , tags :: [Text]
    , title :: Text
    }
    deriving (Generic, FromJSON)

projectDate :: ProjectMeta -> Text
projectDate (ProjectMeta date _ _) = date

parseProject :: FilePath -> IO Project
parseProject fp = do
    putStrLn $ "[+] " <> fp
    ("---" : lines') <- BS.lines <$> BS.readFile fp
    case break (== "---") lines' of
        (yml, "---" : "" : intro : _) -> do
            pm <- decodeThrow (BS.unlines yml)
            pure $ Project fp pm (parseIntro $! decodeUtf8With lenientDecode intro)
        _ -> error $ fp <> ": Missing front matter"

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
    | "code" `elem` p.meta.tags || "keyboard" `elem` p.meta.tags = Just "⌨"
    | otherwise = Nothing

fill_, d_, viewBox_ :: Text -> Attribute
viewBox_ = makeAttribute "viewBox"
fill_ = makeAttribute "fill"
d_ = makeAttribute "d"
path_ :: HtmlT Identity a -> HtmlT Identity a
path_ = makeElement "path"

cliSvg, srvSvg :: Html ()
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
        _ <- Parsec.many1 (Parsec.char '[')
        l <- Text.pack <$> Parsec.many1 (Parsec.satisfy (`notElem` [']']))
        _ <- Parsec.many1 (Parsec.char ']')
        Parsec.optional $ do
            _ <- Parsec.char '('
            _ <- Parsec.many1 (Parsec.satisfy (`notElem` [')']))
            Parsec.char ')'
        Parsec.optional (Parsec.char '#')
        pure $ if Text.elem '|' l then Text.takeWhileEnd (/= '|') l else l

mainProject :: IO ()
mainProject = do
    projFiles <- map (mappend "content/project/") . filter (\fp -> takeExtension fp == ".md") <$> listDirectory "content/project"
    projs <- traverse parseProject projFiles
    renderToFile "content/templates/components/projects.tpl" (renderProjects (reverse $ sortOn (projectDate . projectMeta) projs))
    Process.runProcess_ "deno fmt  ./htmls/projects.html"
