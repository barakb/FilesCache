{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Files where

import           Conduit                      (runConduit, (.|))
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit.Binary          (sinkFile)
import           Numeric                      (showFFloat)

import           Control.Monad.IO.Class       (liftIO)
import qualified Data.ByteString.Char8        as BC (unpack)
import           Data.Maybe                   (listToMaybe, maybe)
import           Network.HTTP.Client          (parseUrlThrow)
import           Network.HTTP.Simple          (getResponseBody,
                                               getResponseHeader,
                                               getResponseStatus, httpSource)
import           Network.HTTP.Types.Status    (statusMessage)

showAsMB ::Int -> String
showAsMB bytes = showFFloat (Just 2) (fromIntegral bytes / (1024 ** 2)) " MB"

url1::String
url1 = "http://hercules/12.3.1/master/19208-19/gigaspaces-xap-enterprise-12.3.1-m9-b19208-19.zip"

downloadURL :: String -> FilePath -> IO ()
downloadURL url location = do
  request <- parseUrlThrow url
  runResourceT
         $ runConduit $  httpSource request processResponse
         .| sinkFile location
   where
     processResponse response = do
         liftIO $ putStrLn $ (BC.unpack . statusMessage . getResponseStatus) response ++ " (" ++ formatSize response ++ ") " ++ url
         getResponseBody response
     formatSize response = maybe "unknown" showAsMB (listToMaybe (getResponseHeader "Content-Length" response) >>= stringToInt . BC.unpack)


stringToInt :: String -> Maybe Int
stringToInt cs =  case reads cs :: [(Int,String)] of
                      [(n, _)] -> Just n
                      _        -> Nothing


