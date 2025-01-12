{-# LANGUAGE NoFieldSelectors #-}

module Medias where

import Data.Aeson (FromJSON, ToJSON, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Read qualified as Text
import Data.Time qualified as Time
import Dhall (FromDhall, ToDhall)
import Dhall qualified
import RIO
import RIO.Vector.Boxed qualified as V
import RIO.Vector.Boxed.Partial qualified as V

import Audio (AudioFile (..), AudioMD (..), AudioMDMeta (..), getAudioFile, release)
import Dhall.Marshal.Encode (Encoder (embed), inject)
import Dhall.Pretty (prettyExpr)
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
    deriving (Generic, Show, FromDhall, ToDhall)

newtype Tag = Tag Text deriving newtype (Show, FromDhall, ToDhall, ToJSON)
newtype Format = Format Text deriving newtype (Show, IsString, Eq, FromDhall, ToDhall, ToJSON)

-- | The extracted metadata from the media file
data MediaAudioMeta = MediaAudioMeta
    { length :: MilliSec
    , freq :: AudioFreq
    , bitDepth :: AudioBitDepth
    }
    deriving (Show, Generic, ToJSON, FromJSON)

-- | disambiguous with length
len :: (Foldable f) => f a -> Int
len = RIO.length

data AudioBitDepth = S16 | S24 | S32 deriving (Show, Generic, ToJSON, FromJSON)
newtype MilliSec = MilliSec Natural deriving newtype (Show, ToJSON, FromJSON)
newtype AudioFreq = AudioFreq Natural deriving newtype (Show, ToJSON, FromJSON)

getAudioMeta :: Media -> IO MediaAudioMeta
getAudioMeta media =
    doesFileExist cache >>= \case
        True -> readJSON cache
        False -> do
            v <-
                doesFileExist legacyMD >>= \case
                    True -> importLegacyAudioMD legacyMD
                    False -> error "NotImplemented: ffprobe decoder"
            Aeson.encodeFile cache v
            pure v
  where
    cache = cdnPath media.path <> ".json"
    legacyMD = cdnPath media.path <> ".md"

importLegacyMedia :: FilePath -> IO Media
importLegacyMedia fp = do
    af <- getAudioFile fp
    let path = Text.pack $ drop (Text.length "/srv/cdn.midirus.com") $ dropExtension fp
    pure
        Media
            { path
            , cover = Nothing
            , format = V.fromList ["flac"]
            , artist = "midirus"
            , title = af.title
            , note = Nothing
            , tags = case af.nfo.meta.playlists of
                Nothing -> mempty
                Just xs -> V.fromList $ fmap Tag xs
            , date = read $ Text.unpack af.release
            }

prettyDhall :: Vector Media -> Text
prettyDhall xs = Text.pack $ show $ prettyExpr (embed inject xs)

importLegacyAudioMD :: FilePath -> IO MediaAudioMeta
importLegacyAudioMD fp = do
    ls <- Text.lines <$> Text.readFile fp
    pure $
        MediaAudioMeta
            (MilliSec $ parseNat (getValue "length: " ls))
            (AudioFreq $ parseNat (getValue "freq: " ls))
            (getFmt (getValue "fmt: " ls))
  where
    getValue name [] = error $ "Could not find " <> show name
    getValue name (x : xs)
        | name `Text.isPrefixOf` x = Text.drop (Text.length name) x
        | otherwise = getValue name xs
    parseNat t = case Text.decimal t of
        Right (x, "") -> x
        e -> error $ "Could not parse " <> show e
    getFmt = \case
        "s16" -> S16
        "s24" -> S24
        "s32" -> S32
        e -> error $ "Unknown fmt: " <> show e

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

readJSON :: (Aeson.FromJSON a) => FilePath -> IO a
readJSON fp = do
    Aeson.eitherDecodeFileStrict fp >>= \case
        Left e -> error e
        Right x -> pure x

-- | Generate the final value to be sent to the browser
processAudio :: Media -> IO Aeson.Value
processAudio m = do
    meta <- getAudioMeta m
    pure $
        object
            [ "path" .= m.path
            , "len" .= meta.length
            , "title" .= m.title
            , "date" .= m.date
            , "fmt" .= m.format
            , "tags" .= m.tags
            ]

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
    allMedias <- getMedias
    mapM_ renderMedias allMedias

    forM_ ["2012-prelude", "2013-andromeda", "2014-raison", "2022-opFreak", "2024-midiFreak"] \n -> do
        m <- mapM importLegacyMedia =<< runFind ["/srv/cdn.midirus.com/audio/" <> n <> "/", "-iname", "*.flac"]
        putStrLn $ "\n-----" <> n
        Text.putStr $ prettyDhall $ V.fromList m
    pure ()
