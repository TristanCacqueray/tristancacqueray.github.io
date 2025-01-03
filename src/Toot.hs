-- | Render the toot.tpl
module Toot where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON)
import Lucid
import RIO
import RIO.Text qualified as Text
import RIO.Vector qualified as Vector
import System.Directory (doesFileExist)
import System.Process.Typed qualified as Process
import Text.Pandoc
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Walk (walk)

inPath, outPath :: FilePath
inPath = "/srv/github.com/miikka/get-my-history/history.json"
outPath = "./content/templates/components/toots.tpl"

renderDate :: Text -> Html ()
renderDate date =
    with div_ [class_ "mr-2 pt-2 text-sm font-semibold font-mono"] do
        toHtml $ Text.take 10 date

renderToot :: Toot -> Html ()
renderToot toot = unless ignored_toot $ with div_ [class_ "flex"] do
    renderDate toot.created_at
    with div_ [class_ "rounded border-2 border-blue-100 px-2 py-1 my-1 grow"] do
        toHtmlRaw $ encodeContent toot_content
        case toot.media_attachments of
            [] -> pure ()
            xs -> br_ [] >> mapM_ renderAttachment xs
  where
    ignored_toot = case toot_content of
        Pandoc _ (Para (Span _ (Span ("", ["mention"], _) _ : _) : _) : _) -> True
        _ -> False
    toot_content = decodeContent toot.content

renderAttachment :: Media -> Html ()
renderAttachment media = case ext of
    "png" -> renderImg
    "jpg" -> renderImg
    "mp3" -> renderAudio
    "mp4" -> renderVideo
    _ -> error $ "Unknown attachment type: " <> show ext
  where
    addAlt = case media.description of
        Nothing -> Prelude.id
        Just desc -> \xs -> title_ desc : alt_ desc : xs
    renderImg = img_ $ addAlt [src_ url]
    renderAudio = audio_ [controls_ "", class_ "lg:w-[750px] mb-4"] do
        source_
            [src_ url, type_ "audio/mpeg"]

    renderVideo = video_ (addAlt [controls_ ""]) do
        source_ [src_ url, type_ "video/mp4"]
    ext = Text.takeWhileEnd (/= '.') media.url
    url = "https://" <> mediaUrl media

renderToots :: Vector Toot -> Html ()
renderToots toots = mapM_ renderToot toots

data Media = Media
    { preview_url :: Maybe Text
    , url :: Text
    , description :: Maybe Text
    }
    deriving (Generic, FromJSON)

data Toot = Toot
    { content :: Text
    , id :: Text
    , in_reply_to_account_id :: Maybe Text
    , created_at :: Text
    , visibility :: Text
    , --    , card :: Maybe Card
      media_attachments :: [Media]
    }
    deriving (Generic, FromJSON)

readHistory :: IO (Vector Toot)
readHistory = either error Prelude.id <$> Aeson.eitherDecodeFileStrict' inPath

keepToot :: Toot -> Bool
keepToot toot =
    toot.visibility == "public" && toot.content /= "" && case toot.in_reply_to_account_id of
        Nothing -> True
        Just acc -> acc == "109396278997132996"

-- Simplify the decoded markup to remove un-necessary para and fixup the links
cleanupMarkup :: Pandoc.Pandoc -> Pandoc.Pandoc
cleanupMarkup (Pandoc meta block) = Pandoc meta newBlocks
  where
    newBlocks =
        walk (removeEmptySpan []) $
            walk removeImplicitLink $
                block

    removeImplicitLink = \case
        Link (lid, classes, lkv) xs (lurl, ls)
            | "hashtag" `elem` classes -> Span ("", ["hashtag"], []) xs
            | "mention" `elem` classes -> Span ("", ["mention"], []) xs
            | otherwise ->
                Link
                    (lid, "text-blue-600" : "hover:underline" : "cursor-pointer" : classes, lkv)
                    xs
                    (shortYT lurl, ls)
        Span ("", classes, []) xs
            | classes `elem` [["invisible"], ["h-card"], ["ellipsis"]] -> Span ("", [], []) xs
        x -> x

    removeEmptySpan :: [Inline] -> [Inline] -> [Inline]
    removeEmptySpan acc [] = reverse acc
    removeEmptySpan acc (x : xs) = case x of
        Span ("", [], []) ss -> removeEmptySpan (reverse ss ++ acc) xs
        _ -> removeEmptySpan (x : acc) xs

    shortYT url = case Text.stripPrefix "https://www.youtube.com/watch?v=" url of
        Nothing -> url
        Just yt -> "https://youtu.be/" <> Text.take 11 yt <> if Text.length yt > 11 then "?" <> Text.drop 12 yt else ""

decodeContent :: Text -> Pandoc.Pandoc
decodeContent htmlTxt =
    cleanupMarkup $
        either (error . show) Prelude.id $
            Pandoc.runPure $
                Pandoc.readHtml (Pandoc.def{readerExtensions = Pandoc.pandocExtensions}) htmlTxt

encodeContent :: Pandoc -> Text
encodeContent pdc = either (error . show) Prelude.id $ Pandoc.runPure $ Pandoc.writeHtml5String (Pandoc.def{writerExtensions = Pandoc.pandocExtensions}) pdc

processContent :: Text -> Text
processContent = encodeContent . decodeContent

syncUrl :: Toot -> IO ()
syncUrl toot = mapM_ syncAttachment toot.media_attachments
  where
    syncAttachment :: Media -> IO ()
    syncAttachment media = do
        let ofp = Text.unpack $ "/srv/" <> mediaUrl media
        unlessM (doesFileExist ofp) do
            let args = ["-o", ofp, Text.unpack media.url]
            print args
            Process.runProcess_ $ Process.proc "curl" args

mediaUrl :: Media -> Text
mediaUrl media = "cdn.midirus.com/medias/" <> Text.takeWhileEnd (/= '/') media.url

mainToot :: IO ()
mainToot = do
    -- print $ decodeContent "<p>..."
    toots <- Vector.reverse . Vector.filter keepToot <$> readHistory
    mapM_ syncUrl toots
    renderToFile outPath $ renderToots toots
    Process.runProcess_ "deno fmt ./htmls/toots.html"
