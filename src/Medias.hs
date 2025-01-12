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

import Audio (MediaAudioMeta (..), getAudioMeta)
import System.Directory (createDirectoryIfMissing, doesFileExist)
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

-- | disambiguous with length
len :: (Foldable f) => f a -> Int
len = RIO.length

notesRoot :: FilePath
notesRoot = "/srv/code.midirus.com/website/content"

getMedias :: IO [(FilePath, Vector Media)]
getMedias = do
    files <- runFind [notesRoot, "-name", "*.dhall"]
    medias <- mapM readMedias files
    pure $ zip (map (drop (Prelude.length notesRoot)) files) medias

renderMedias :: (FilePath, Vector Media) -> IO (Text, Aeson.Value)
renderMedias (fp, medias) = do
    json <- V.mapM processMedia medias
    let noteUrl = dropExtension fp
    let outFile = "/srv/cdn.midirus.com" <> noteUrl <> ".json"
    let outDir = takeDirectory outFile
    createDirectoryIfMissing True outDir
    print (outFile, outDir)
    Aeson.encodeFile outFile json
    pure (Text.pack noteUrl, Aeson.Array json)

readMedias :: FilePath -> IO (Vector Media)
readMedias = Dhall.input Dhall.auto . Text.pack

cdnPath :: Text -> FilePath
cdnPath t = Text.unpack $ "/srv/cdn.midirus.com/" <> t

-- | Generate the final value to be sent to the browser
processAudio :: Media -> IO Aeson.Value
processAudio m = do
    meta <- getAudioMeta $ cdnPath m.path <> audioFmt
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

writeAudioPlaylist :: FilePath -> Vector Media -> IO ()
writeAudioPlaylist fp media = do
    values <- V.mapM processAudio media
    Aeson.encodeFile fp values

processMedia :: Media -> IO Aeson.Value
processMedia m
    | "flac" `V.elem` m.format || "mp3" `V.elem` m.format = processAudio m
    | otherwise = error $ show m.format <> ": process not implemented"

test :: IO ()
test = do
    -- writeAudioPlaylist "/srv/cdn.midirus.com/audio/pasta.json" =<< readMedias "/srv/code.midirus.com/website/content/project/pastagang.dhall"
    -- allMedias <- getMedias
    -- mapM_ renderMedias allMedias

    pure ()
