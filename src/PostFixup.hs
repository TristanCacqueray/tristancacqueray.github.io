module PostFixup (mainPostFixup) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSC (pack)
import Data.Digest.Pure.SHA qualified as SHA
import Data.List (isSuffixOf)

import Control.Monad (when)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import RIO.Directory (getModificationTime, setModificationTime)
import RIO.FilePath (dropExtension)
import RIO.Prelude (whenM)
import System.Directory (doesFileExist)

import Utils

-- Fixup file timestamps
mainPostFixup :: IO ()
mainPostFixup = do
    -- Hash the generated tailwind
    cssHash <- BS.toStrict . LBSC.pack . SHA.showDigest . SHA.sha1 <$> LBS.readFile "/srv/midirus.com/tailwind.css"

    -- Process the generated files
    outputFiles <- runFind ["/srv/midirus.com/"]
    mapM_ (fixOutput cssHash) outputFiles

    -- Set the timestamp based on the source files
    -- TODO: do that in ema directlry?
    inputFiles <- runFind ["content/", "-type", "f"]
    mapM_ setTS inputFiles

    putStrLn "Done with post fixup"
  where
    fixOutput cssHash fp = do
        when (".html" `isSuffixOf` fp) do
            -- Patch the tailwind instance id to not be random
            content <- BS.readFile fp
            let (prev, next) = BS.breakSubstring "tailwind.css?instanceId=" content
            BS.writeFile fp $ mconcat [prev, "tailwind.css?instanceId=", cssHash, BSC.dropWhile (/= '\'') next]
        setModificationTime fp $ posixSecondsToUTCTime $ fromIntegral (0 :: Int)
    setTS fp = do
        ts <- getModificationTime fp
        let outFP = "/srv/midirus.com/" <> (drop 8 fp)
        doesFileExist outFP >>= \case
            True -> do
                setModificationTime outFP ts
            False -> do
                let baseFP = dropExtension outFP
                whenM (doesFileExist $ baseFP <> ".html") do
                    setModificationTime (baseFP <> ".html") ts
                whenM (doesFileExist (baseFP <> ".xml")) do
                    setModificationTime (baseFP <> ".xml") ts
