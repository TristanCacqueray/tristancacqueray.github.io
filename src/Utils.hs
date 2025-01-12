module Utils where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.List (isSuffixOf)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Time
import System.Directory (doesFileExist, getModificationTime)
import System.Process.Typed qualified as P
import Text.Pandoc (extensionsFromList, runIOorExplode)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Extensions (Extension (Ext_auto_identifiers))
import Text.Pandoc.Options (def, readerExtensions)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Readers.Org (readOrg)

runFind :: [String] -> IO [FilePath]
runFind args = toFP <$> P.readProcessStdout_ (P.proc "find" args)
  where
    toFP = map Text.unpack . Text.lines . Text.decodeUtf8 . BS.toStrict

readPandoc :: FilePath -> IO Pandoc
readPandoc fp = do
    content <- Text.readFile fp
    runIOorExplode $
        if ".org" `isSuffixOf` fp
            then readOrg readerOpts content
            else readMarkdown readerOpts content
  where
    readerOpts = def{readerExtensions = extensionsFromList exts}
    exts = [Ext_auto_identifiers]

readJSON :: (Aeson.FromJSON a) => FilePath -> IO a
readJSON fp = do
    Aeson.eitherDecodeFileStrict fp >>= \case
        Left e -> error e
        Right x -> pure x

-- | Check if the fp needs to be updated, either because it is missing or too old
isOutdated :: UTCTime -> FilePath -> IO Bool
isOutdated ts fp = do
    doesFileExist fp >>= \case
        False -> pure True
        True -> do
            modTime <- getModificationTime fp
            pure (ts > modTime)
