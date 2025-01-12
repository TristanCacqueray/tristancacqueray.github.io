{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid reverse" #-}
{-# HLINT ignore "Redundant bracket" #-}

-- | Render the audio.json
module Audio where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Char (toTitle)
import Data.List.Split
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Yaml hiding (Parser, encodeFile)
import RIO
import System.Directory (doesPathExist, getModificationTime)
import System.FilePath
import System.Process.Typed qualified as P

import Utils

-- | The extracted metadata from the media file
data MediaAudioMeta = MediaAudioMeta
    { length :: MilliSec
    , freq :: AudioFreq
    , bitDepth :: AudioBitDepth
    }
    deriving (Show, Generic, ToJSON, FromJSON)

data AudioBitDepth = S16 | S24 | S32 deriving (Show, Generic, ToJSON, FromJSON)
newtype MilliSec = MilliSec Natural deriving newtype (Show, ToJSON, FromJSON)
newtype AudioFreq = AudioFreq Natural deriving newtype (Show, ToJSON, FromJSON)

readBitDepth :: Text -> AudioBitDepth
readBitDepth = \case
    "s16" -> S16
    "s24" -> S24
    "s32" -> S32
    n -> error $ "Unknown bit depth: " <> show n

getAudioMeta :: FilePath -> IO MediaAudioMeta
getAudioMeta fp = do
    ts <- getModificationTime fp
    whenM (isOutdated ts peaksPath) do
        P.runProcess_ $ P.proc "audiowaveform" ["-i", fp, "-o", peaksPath, "-b", "8"]
    isOutdated ts cache >>= \case
        False -> readJSON cache
        True -> do
            v <- ffprobe fp
            Aeson.encodeFile cache v
            pure v
  where
    basePath = dropExtension fp
    cache = basePath <> ".json"
    peaksPath = basePath <> ".dat"

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
    }
    deriving (Show, Generic, ToJSON)

mainAudio :: IO ()
mainAudio = do
    -- traverse_ print files
    -- traverse_ print audioFiles
    -- encodeFile "/srv/cdn.midirus.com/audio.json" $ renderAudioMetaData audioFiles
    putStrLn "Done!"

getAudioFile :: FilePath -> IO AudioFile
getAudioFile fp = do
    let basePath = dropExtension fp
        path = Text.pack $ joinPath $ reverse $ take 2 $ reverse $ splitDirectories basePath
        mdPath = basePath <> ".md"
    modTime <- getModificationTime fp
    let date = Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d" modTime
    unlessM (doesPathExist (basePath <> ".mp3")) do
        encodeFlac (takeWhile (/= '-') (Text.unpack date)) basePath
    pure
        AudioFile
            { path
            , title = Text.pack $ trackName fp
            , album = Text.pack $ albumName fp
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

toCapitalize :: [Char] -> String
toCapitalize = unwords . map capitalizeWord . splitWhen (`elem` ['-', '_'])

capitalizeWord :: String -> String
capitalizeWord = \case
    "yul" -> "yul"
    t : rest -> toTitle t : rest
    name -> name

renderAudioMetaData :: [AudioFile] -> AudioMetaData
renderAudioMetaData files = AudioMetaData albums playlists files
  where
    albums = []
    playlists = []

ffprobe :: FilePath -> IO MediaAudioMeta
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
        MediaAudioMeta
            { length = MilliSec $ ffprobe2MSec (Text.unpack $ head duration)
            , freq = AudioFreq $ read $ Text.unpack $ head freq
            , bitDepth = readBitDepth $ head fmt
            }

{- | Convert ffprobe timetamp to MS

>>> ffprobe2MSec "00:00:01:23,"
1230
-}
ffprobe2MSec :: String -> Natural
ffprobe2MSec s0 = case s0 of
    h2 : h1 : ':' : m2 : m1 : ':' : s2 : s1 : '.' : ms2 : ms1 : [','] ->
        1000 * ((mkDigit h2 h1) * 3600 + (mkDigit m2 m1) * 60 + (mkDigit s2 s1))
            + (mkDigit ms2 ms1) * 10
    _ -> error $ "bad ts: " <> s0

mkDigit :: Char -> Char -> Natural
mkDigit c2 c1 = fromInteger $ toInteger $ (fromEnum c2 - 48) * 10 + (fromEnum c1 - 48)

getValue :: Text -> [Text] -> [Text]
getValue name = drop 1 . dropWhile (/= name)
