{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Files where

import           Conduit                      (runConduit, (.|))
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit.Binary          (sinkFile)
import           Numeric                      (showFFloat)

import           Control.Monad.IO.Class       (liftIO)
import qualified Data.ByteString.Char8        as BC (unpack, null)
import           Data.Maybe                   (listToMaybe, maybe)
import           Network.HTTP.Simple          (getResponseBody,
                                               getResponseHeader,
                                               getResponseStatus, httpSource,
                                               parseRequest)
import           Network.HTTP.Types.Status    (statusMessage, statusIsSuccessful)


showAsMB ::Int -> String
showAsMB bytes = showFFloat (Just 2) (fromIntegral bytes / (1024 ** 2)) " MB"

url1::String
url1 = "http://hercules/12.3.1/master/19208-19/gigaspaces-xap-enterprise-12.3.1-m9-b19208-19.zip"

downloadURL :: String -> FilePath -> IO ()
downloadURL url location = do
  request <- parseRequest url
  runResourceT
         $ runConduit$  httpSource request getSrc
         .| sinkFile location
   where
     getSrc res = do
         liftIO $ putStrLn $ (BC.unpack . statusMessage . getResponseStatus) res ++ " (" ++ formatSize res ++ ") " ++ url
         let status = statusIsSuccessful . getResponseStatus $ res
         if status then
            getResponseBody res
         else
            return ()
     formatSize res = maybe "unknown" showAsMB (listToMaybe (getResponseHeader "Content-Length" res) >>= stringToInt . BC.unpack)


stringToInt :: String -> Maybe Int
stringToInt cs =  case reads cs :: [(Int,String)] of
                      [(n, _)] -> Just n
                      _        -> Nothing


