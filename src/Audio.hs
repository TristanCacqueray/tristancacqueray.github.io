{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid reverse" #-}
{-# HLINT ignore "Redundant bracket" #-}

-- | Render the audio.json
module Audio where

import Data.Aeson (encodeFile)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toTitle)
import Data.List (sortBy)
import Data.List.Split
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Yaml hiding (Parser, encodeFile)
import Data.Yaml qualified as Yaml
import RIO
import System.Directory (doesPathExist, getModificationTime)
import System.FilePath
import System.Process.Typed qualified as P

import Utils

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
    , playlists :: Maybe [Text]
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

mainAudio :: IO ()
mainAudio = do
    files <- runFind ["/srv/cdn.midirus.com/audio", "-name", "*.flac"]
    -- traverse_ print files
    audioFiles <- reverse . sortBy orderFiles <$> traverse getAudioFile files
    -- traverse_ print audioFiles
    encodeFile "/srv/cdn.midirus.com/audio.json" $ renderAudioMetaData audioFiles
    encodeFile "/srv/cdn.midirus.com/audio/pastagang.json" $ filter isPasta audioFiles
    putStrLn "Done!"

isPasta :: AudioFile -> Bool
isPasta af = af.album == "Pastagang"

orderFiles :: AudioFile -> AudioFile -> Ordering
orderFiles f1 f2 = case compare (albumDate f1.path) (albumDate f2.path) of
    EQ -> case compare (fromMaybe 9999 f1.nfo.meta.pos) (fromMaybe 9999 f2.nfo.meta.pos) of
        EQ -> compare f1.nfo.meta.date f2.nfo.meta.date
        o -> o
    o -> o
  where
    albumDate = Text.takeWhile (/= '/')

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
    unlessM (doesPathExist (basePath <> ".dat")) do
        P.runProcess_ $ P.proc "audiowaveform" ["-i", fp, "-o", basePath <> ".dat", "-b", "8"]
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
    albums = map toPlaylist $ snd $ foldl mkAlbum (0, []) files
    playlists = map toPlaylist $ snd $ foldl mkPlaylist (0, []) files

toPlaylist :: (Text, [Word]) -> Playlist
toPlaylist (name, songs) = Playlist name (reverse songs)

mkAlbum :: (Word, [(Text, [Word])]) -> AudioFile -> (Word, [(Text, [Word])])
mkAlbum (pos, acc) af = (pos + 1, insertSong pos acc af.album)

mkPlaylist :: (Word, [(Text, [Word])]) -> AudioFile -> (Word, [(Text, [Word])])
mkPlaylist (pos, acc) af = (pos + 1, newAcc)
  where
    newAcc = foldl (insertSong pos) acc (Text.pack . toCapitalize . Text.unpack <$> fromMaybe [] af.nfo.meta.playlists)

insertSong :: Word -> [(Text, [Word])] -> Text -> [(Text, [Word])]
insertSong pos [] name = [(name, [pos])]
insertSong pos ((pname, songs) : rest) name
    | name == pname = (name, pos : songs) : rest
    | otherwise = (pname, songs) : insertSong pos rest name

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
ffprobe2MSec s0 = case s0 of
    h2 : h1 : ':' : m2 : m1 : ':' : s2 : s1 : '.' : ms2 : ms1 : [','] ->
        1000 * ((mkDigit h2 h1) * 3600 + (mkDigit m2 m1) * 60 + (mkDigit s2 s1))
            + (mkDigit ms2 ms1) * 10
    _ -> error $ "bad ts: " <> s0

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
                        , playlists = Nothing
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
    ("---" : lines') <- BS.lines <$> BS.readFile fp
    case break (== "---") lines' of
        (yml, "---" : intro) -> do
            pm <- decodeThrow (BS.unlines yml)
            pure $ AudioMD pm (decodeUtf8With lenientDecode $ BS8.dropWhile (`elem` [' ', '\n']) $ BS.unlines intro)
        _ -> error $ fp <> ": missing frontmatter"
