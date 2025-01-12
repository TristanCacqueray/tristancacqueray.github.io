{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid reverse" #-}
{-# HLINT ignore "Redundant bracket" #-}

-- | Render the audio.json
module Audio where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Yaml hiding (Parser, encodeFile)
import RIO
import System.Directory (doesFileExist, getModificationTime, setModificationTime)
import System.FilePath
import System.Process.Typed qualified as P

import Utils

data ID3Tags = ID3Tags
    { artist :: String
    , title :: String
    , date :: String
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | The extracted metadata from the media file
data AudioFormatInfo = AudioFormatInfo
    { length :: MilliSec
    , freq :: AudioFreq
    , bitDepth :: AudioBitDepth
    , id3Tags :: Maybe ID3Tags
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

getAudioMeta :: ID3Tags -> FilePath -> IO AudioFormatInfo
getAudioMeta tags fp = do
    ts <- getModificationTime fp

    -- extract metadata from cache or with ffprobe
    fmtInfo <-
        isOutdated ts cache >>= \case
            False -> readJSON cache
            True -> do
                v <- ffprobe tags fp
                Aeson.encodeFile cache v
                pure v

    when (fmtInfo.id3Tags /= Just tags) do
        -- update file tags
        whenM (doesFileExist flacPath) do
            putStrLn "Updating flac"
            writeFLACMetadata tags flacPath
        whenM (doesFileExist mp3Path) do
            putStrLn "Updating mp3"
            writeMP3Metadata tags mp3Path
        Aeson.encodeFile cache $ fmtInfo{id3Tags = Just tags}

    -- generate waveform data for peaks.js
    let peaksPath = basePath <> ".dat"
    whenM (isOutdated ts peaksPath) do
        P.runProcess_ $ P.proc "audiowaveform" ["-i", fp, "-o", peaksPath, "-b", "8"]

    -- generate mp3 from flac
    whenM (doesFileExist flacPath) do
        whenM (isOutdated ts mp3Path) do
            encodeMp3 fp mp3Path

    pure fmtInfo
  where
    basePath = dropExtension fp
    cache = basePath <> ".json"
    mp3Path = basePath <> ".mp3"
    flacPath = basePath <> ".flac"

writeMP3Metadata :: ID3Tags -> FilePath -> IO ()
writeMP3Metadata tags fp = do
    ts <- getModificationTime fp
    P.runProcess_ $ P.proc "id3v2" args
    setModificationTime fp ts
  where
    args =
        [ "-D"
        , "--artist"
        , tags.artist
        , "--song"
        , tags.title
        , "--year"
        , tags.date
        ]

writeFLACMetadata :: ID3Tags -> FilePath -> IO ()
writeFLACMetadata tags fp = P.runProcess_ $ P.proc "metaflac" args
  where
    args =
        [ "--preserve-modtime"
        , "--remove-all-tags"
        , "--set-tag=artist=" <> tags.artist
        , "--set-tag=title=" <> tags.title
        , "--set-tag=year=" <> tags.date
        , fp
        ]

encodeMp3 :: FilePath -> FilePath -> IO ()
encodeMp3 ifp ofp = do
    P.runProcess_ $ P.proc "ffmpeg" ffmpegArgs
    -- preserve timestamps
    P.runProcess_ $ P.proc "touch" ["-r", ifp, ofp]
  where
    ffmpegArgs = ["-hide_banner", "-i", ifp, "-map_metadata", "0", "-id3v2_version", "3", "-ar", "44100", ofp]

ffprobe :: FilePath -> IO AudioFormatInfo
ffprobe fp = do
    putStrLn $ fp <> ": running ffprobe"
    (P.ExitSuccess, meta) <- P.readProcessStderr $ P.proc "ffprobe" ["-hide_banner", fp]
    -- TODO: fix this mess, that's ugly
    let duration = getValue "Duration:" (Text.words $ Text.decodeUtf8 $ BS.toStrict meta)
    let freq = getValue "flac," duration
    let fmt = case getValue "stereo," freq of
            [] -> getValue "mono," freq
            o -> o
    pure $
        AudioFormatInfo
            { length = MilliSec $ ffprobe2MSec (Text.unpack $ head duration)
            , freq = AudioFreq $ read $ Text.unpack $ head freq
            , bitDepth = readBitDepth $ head fmt
            , id3Tags = Nothing -- todo, read the tags from the probe
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
