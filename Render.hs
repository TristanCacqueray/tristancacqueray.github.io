-- \| Render.hs is a program to generate custom template:
--
-- - projects list from directory listing
-- - snippets from org mode file
--
-- nix develop --command ghcid ./Render.hs -T main
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Data.Aeson (FromJSON, encodeFile)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (isDigit, toTitle)
import Data.List (isSuffixOf, sortBy, sortOn)
import Data.List.Split
import Data.Map qualified as Map
import Data.String.QQ (s)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Tree
import Data.Yaml hiding (Parser, encodeFile)
import Data.Yaml qualified as Yaml
import GHC.Float (int2Float)
import Lucid
import Lucid.Base (makeAttribute, makeElement, makeElementNoEnd)
import RIO
import RIO.Text qualified
import System.Directory (doesPathExist, getModificationTime, listDirectory)
import System.FilePath
import System.Process (callProcess, readProcess)
import System.Process.Typed qualified as P
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Definition
import Text.Pandoc.Extensions
import Text.Pandoc.Options (def, readerExtensions)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Readers.Org (readOrg)
import Text.Parsec qualified as Parsec
import Text.Parsec.Text (Parser)

mainAudio :: IO ()
mainAudio = do
    files <- runFind ["/srv/cdn.midirus.com/audio", "-name", "*.flac"]
    -- traverse_ print files
    audioFiles <- reverse . sortBy orderFiles <$> traverse getAudioFile files
    -- traverse_ print audioFiles
    encodeFile "/srv/cdn.midirus.com/audio.json" $ renderAudioMetaData audioFiles
    print "Done!"

orderFiles :: AudioFile -> AudioFile -> Ordering
orderFiles f1 f2 = case compare (albumDate f1.path) (albumDate f2.path) of
    EQ -> case compare (fromMaybe 9999 f1.nfo.meta.pos) (fromMaybe 9999 f2.nfo.meta.pos) of
        EQ -> compare f1.nfo.meta.date f2.nfo.meta.date
        o -> o
    o -> o
  where
    albumDate = Text.takeWhile (/= '/')

runFind :: [String] -> IO [FilePath]
runFind args = toFP <$> P.readProcessStdout_ (P.proc "find" args)
  where
    toFP = map Text.unpack . Text.lines . Text.decodeUtf8 . BS.toStrict

getAudioFile :: FilePath -> IO AudioFile
getAudioFile fp = do
    let basePath = dropExtension fp
        path = Text.pack $ joinPath $ reverse $ take 2 $ reverse $ splitDirectories basePath
        mdPath = basePath <> ".md"
    modTime <- getModificationTime fp
    let date = Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d" modTime
    audioMD <- getAudioMD fp mdPath date
    unlessM (doesPathExist (basePath <> ".mp3")) do
      encodeFlac (takeWhile (/= '-') (Text.unpack date)) basePath
    pure
        AudioFile
            { path
            , title = Text.pack $ trackName fp
            , album = Text.pack $ albumName fp
            , nfo = audioMD
            , release = date
            }

encodeFlac :: String -> FilePath -> IO ()
encodeFlac year basePath = do
    let args = ["--preserve-modtime", "--remove-all-tags", "--set-tag=artist=Midirus", "--set-tag=album=" <> album, "--set-tag=year=" <> year, "--set-tag=title=" <> title, basePath <> ".flac"]
        ffmpegArgs = ["-hide_banner", "-i", basePath <> ".flac", "-map_metadata", "0", "-id3v2_version", "3", "-ar", "44100", basePath <> ".mp3"]
    -- print args >> print ffmpegArgs >> error "stop"
    P.runProcess_ $ P.proc "metaflac" args
    P.runProcess_ $ P.proc "ffmpeg" ffmpegArgs
    P.runProcess_ $ P.proc "touch" ["-r", basePath <> ".flac", basePath <> ".mp3"]
  where
    title = trackName basePath
    album = albumName basePath

trackName, albumName :: FilePath -> String
trackName = toCapitalize . takeBaseName
albumName = toCapitalize . drop 1 . dropWhile (/= '-') . takeBaseName . takeDirectory

toCapitalize = unwords . map capitalizeWord . splitWhen (`elem` ['-', '_'])

capitalizeWord :: String -> String
capitalizeWord = \case
    "yul" -> "yul"
    t : rest -> toTitle t : rest
    name -> name

getAudioMDs :: IO [AudioMD]
getAudioMDs = pure []

renderAudioMetaData :: [AudioFile] -> AudioMetaData
renderAudioMetaData files = AudioMetaData albums playlists files
  where
    albums = map toPlaylist $ snd $ foldl mkAlbum (0, []) files
    playlists = [] -- map toPlaylist $ snd $ foldl mkPlaylist (0, []) (map fst xs)

toPlaylist :: (Text, [Word]) -> Playlist
toPlaylist (name, songs) = Playlist name (reverse songs)

mkAlbum :: (Word, [(Text, [Word])]) -> AudioFile -> (Word, [(Text, [Word])])
mkAlbum (pos, acc) af = (pos + 1, insertSong pos acc af.album)

{-
mkPlaylist :: (Word, [(Text, [Word])]) -> AudioFileInfo -> (Word, [(Text, [Word])])
mkPlaylist (pos, acc) aif = (pos + 1, newAcc)
  where
    newAcc = foldl (insertSong pos) acc (fromMaybe [] aif.playlists)
-}

insertSong :: Word -> [(Text, [Word])] -> Text -> [(Text, [Word])]
insertSong pos [] name = [(name, [pos])]
insertSong pos ((pname, songs) : rest) name
    | name == pname = (name, pos : songs) : rest
    | otherwise = (pname, songs) : insertSong pos rest name

data AudioMetaData = AudioMetaData
    { albums :: [Playlist]
    , playlists :: [Playlist]
    , files :: [AudioFile]
    }
    deriving (Generic, ToJSON)

data Playlist = Playlist
    { name :: Text
    , sounds :: [Word]
    }
    deriving (Generic, ToJSON)

data AudioFile = AudioFile
    { path :: Text
    , title :: Text
    , album :: Text
    , release :: Text
    , nfo :: AudioMD
    }
    deriving (Show, Generic, ToJSON)

-- | The audio file note front matter
data AudioMDMeta = AudioMDMeta
    { rating :: Maybe Natural
    , pos :: Maybe Natural
    , length :: Natural
    , freq :: Natural
    , fmt :: Text
    , date :: Text
    }
    deriving (Generic, FromJSON, ToJSON, Show)

data AudioMD = AudioMD
    { meta :: AudioMDMeta
    , body :: Text
    }
    deriving (Show, Generic, ToJSON)

data FFProbeMeta = FFProbeMeta
    { length :: Natural
    , freq :: Natural
    , format :: Text
    }
    deriving (Show)

ffprobe :: FilePath -> IO FFProbeMeta
ffprobe fp = do
    putStrLn $ fp <> ": running ffprobe"
    (P.ExitSuccess, meta) <- P.readProcessStderr $ P.proc "ffprobe" ["-hide_banner", fp]
    print meta
    let duration = getValue "Duration:" (Text.words $ Text.decodeUtf8 $ BS.toStrict meta)
    let freq = getValue "flac," duration
    let fmt = case getValue "stereo," freq of
          [] -> getValue "mono," freq
          o -> o
    pure $
        FFProbeMeta
            { length = ffprobe2MSec (Text.unpack $ head duration)
            , freq = read $ Text.unpack $ head freq
            , format = head fmt
            }

{- | Convert ffprobe timetamp to MS

>>> ffprobe2MSec "00:00:01:23,"
1230
-}
ffprobe2MSec :: String -> Natural
ffprobe2MSec s = case s of
    h2 : h1 : ':' : m2 : m1 : ':' : s2 : s1 : '.' : ms2 : ms1 : [','] ->
        1000 * ((mkDigit h2 h1) * 3600 + (mkDigit m2 m1) * 60 + (mkDigit s2 s1))
            + (mkDigit ms2 ms1) * 10
    _ -> error $ "bad ts: " <> s

mkDigit :: Char -> Char -> Natural
mkDigit c2 c1 = fromInteger $ toInteger $ (fromEnum c2 - 48) * 10 + (fromEnum c1 - 48)

getValue :: Text -> [Text] -> [Text]
getValue name = drop 1 . dropWhile (/= name)

getAudioMD :: FilePath -> FilePath -> Text -> IO AudioMD
getAudioMD sfp mdPath date =
    doesPathExist mdPath >>= \case
        True -> parseAudioMD mdPath
        False -> do
            meta <- ffprobe sfp
            let audioMeta =
                    AudioMDMeta
                        { rating = Nothing
                        , pos = Nothing
                        , length = meta.length
                        , freq = meta.freq
                        , fmt = meta.format
                        , date
                        }
            let audioMD = AudioMD audioMeta ""
            updateAudioMD mdPath audioMD
            pure audioMD

updateAudioMD :: FilePath -> AudioMD -> IO ()
updateAudioMD fp audioMD =
    BS.writeFile fp $
        BS.unlines
            [ "---"
            , Yaml.encode audioMD.meta
                <> "---"
            , ""
            , encodeUtf8 audioMD.body
            ]

parseAudioMD :: FilePath -> IO AudioMD
parseAudioMD fp = do
    -- putStrLn $ "[+] " <> fp
    ("---" : lines) <- BS.lines <$> BS.readFile fp
    let (yml, "---" : intro) = break (== "---") lines
    pm <- decodeThrow (BS.unlines yml)
    pure $ AudioMD pm (decodeUtf8With lenientDecode $ BS8.dropWhile (`elem` [' ', '\n']) $ BS.unlines intro)

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
    | "code" `elem` p.meta.tags || "keyboard" `elem` p.meta.tags = Just "⌨"
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

mainProjs :: IO ()
mainProjs = do
    projFiles <- map (mappend "content/project/") <$> listDirectory "content/project"
    projs <- traverse parseProject projFiles
    renderToFile "content/templates/components/projects.tpl" (renderProjects (reverse $ sortOn (projectDate . projectMeta) projs))

-- (id, title)
data DocHeading = DocHeading Text Text

pandocTOCTree :: Pandoc -> Forest DocHeading
pandocTOCTree (Pandoc _ blocks) = go [] 1 blocks
  where
    go acc _lvl [] = reverse acc
    go acc lvl (x : rest) = case x of
        Header hlvl (oid, _, _) [Str title]
            | hlvl == lvl ->
                -- TODO: support nested heading
                let oh = DocHeading oid title
                    childs = []
                 in go (Node oh childs : acc) lvl rest
        _ -> go acc lvl rest

renderTOCTree :: Text -> Tree DocHeading -> Html ()
renderTOCTree base (Node (DocHeading oid title) childs) = li_ do
    with a_ [href_ (mconcat [base, "#", oid])] do
        (toHtml title)
    unless (null childs) do
        ul_ do
            traverse_ (renderTOCTree base) childs

doRead :: FilePath -> IO Pandoc
doRead fp = do
    content <- Text.readFile fp
    runIOorExplode $
        if ".org" `isSuffixOf` fp
            then readOrg readerOpts content
            else readMarkdown readerOpts content
  where
    readerOpts = def{readerExtensions = extensionsFromList (exts)}
    exts = [Ext_auto_identifiers]

renderTOCTemplate :: FilePath -> Pandoc -> IO ()
renderTOCTemplate fp doc = do
    renderToFile fp do
        ul_ do
            traverse_ (renderTOCTree "snippets") (pandocTOCTree doc)

mainSnippets :: IO ()
mainSnippets = do
    renderTOCTemplate "content/templates/components/toc-snippets.tpl" =<< doRead "content/snippets.org"

main :: IO ()
main = mainAudio -- mainProjs >> mainSnippets
