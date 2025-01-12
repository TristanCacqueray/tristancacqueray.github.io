{-# LANGUAGE NoFieldSelectors #-}

module Medias where

import Data.Aeson (ToJSON, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Time qualified as Time
import Dhall (FromDhall)
import Dhall qualified
import RIO
import RIO.Vector.Boxed qualified as V

import Audio (AudioFormatInfo (..), ID3Tags (..), getAudioMeta)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropExtension, takeDirectory)
import Utils

-- | The user provided schema
data Media = Media
    { path :: Text
    , cover :: Maybe Text
    , format :: Vector Format
    , artist :: Text
    , title :: Text
    , date :: Time.Day
    , note :: Maybe Text
    , tags :: Vector Tag
    }
    deriving (Generic, Show, FromDhall)

newtype Tag = Tag Text deriving newtype (Show, FromDhall, ToJSON)
newtype Format = Format Text deriving newtype (Show, IsString, Eq, FromDhall, ToJSON)

data Release = Release
    { name :: Text
    , medias :: Vector Media
    }
    deriving (Generic, Show, FromDhall)

data ReleaseExport = ReleaseExport
    { name :: Text
    , note :: Text
    , medias :: Vector Aeson.Value
    }
    deriving (Generic, Show, ToJSON)

-- | disambiguous with length
len :: (Foldable f) => f a -> Int
len = RIO.length

notesRoot :: FilePath
notesRoot = "/srv/code.midirus.com/website/content"

getReleases :: IO [(FilePath, Release)]
getReleases = do
    files <- runFind [notesRoot, "-name", "*.dhall"]
    releases <- mapM readRelease files
    pure $ zip (map (drop (Prelude.length notesRoot)) files) releases

renderRelease :: (FilePath, Release) -> IO ReleaseExport
renderRelease (fp, release) = do
    json <- V.mapM (processMedia release) release.medias
    let noteUrl = dropExtension fp
    let outFile = "/srv/cdn.midirus.com" <> noteUrl <> ".json"
    let outDir = takeDirectory outFile
    createDirectoryIfMissing True outDir
    writeJSON outFile json
    pure $
        ReleaseExport
            { name = release.name
            , note = Text.pack noteUrl
            , medias = json
            }

readRelease :: FilePath -> IO Release
readRelease = Dhall.input Dhall.auto . Text.pack

cdnPath :: Text -> FilePath
cdnPath t = Text.unpack $ "/srv/cdn.midirus.com/" <> t

-- | Generate the final value to be sent to the browser
processAudio :: Release -> Media -> IO Aeson.Value
processAudio r m = do
    let tags =
            ID3Tags
                { artist = Text.unpack m.artist
                , title = Text.unpack m.title
                , date = show m.date
                , album = Just $ Text.unpack r.name
                }
    meta <- getAudioMeta tags $ cdnPath m.path <> audioFmt
    pure $
        object
            [ "path" .= m.path
            , "len" .= meta.length
            , "title" .= m.title
            , "date" .= m.date
            , "fmt" .= m.format
            , "tags" .= m.tags
            ]
  where
    audioFmt
        | "flac" `V.elem` m.format = ".flac"
        | "mp3" `V.elem` m.format = ".mp3"
        | otherwise = error $ "unknown audio format! " <> show m.format

writeAudioPlaylist :: FilePath -> Release -> IO ()
writeAudioPlaylist fp release = do
    values <- V.mapM (processAudio release) release.medias
    Aeson.encodeFile fp values

processMedia :: Release -> Media -> IO Aeson.Value
processMedia r m
    | "flac" `V.elem` m.format || "mp3" `V.elem` m.format = processAudio r m
    | otherwise = error $ show m.format <> ": process not implemented"

mainMedia :: IO ()
mainMedia = do
    releases <- traverse renderRelease =<< getReleases
    writeJSON "/srv/cdn.midirus.com/medias.json" releases
    putStrLn "Medias completed!"
