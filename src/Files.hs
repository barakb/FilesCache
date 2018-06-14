{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Files where

import           Conduit                      (runConduit, (.|))
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit.Binary          (sinkFile)
import           Numeric                      (showFFloat)

import           Control.Monad.IO.Class       (liftIO)
import qualified Data.ByteString.Char8        as BC (unpack)
import           Data.Maybe                   (listToMaybe, maybe)
import           Network.HTTP.Client          (parseUrlThrow)
import           Network.HTTP.Simple          (getResponseBody, getResponseHeader, httpSource)
import           Control.Monad.Reader(MonadReader, MonadIO, asks)
import           Env (Env(..))


showAsMB ::Int -> String
showAsMB bytes = showFFloat (Just 2) ((fromIntegral bytes / (1024 ** 2))::Double) " MB"

downloadURL ::  (MonadReader Env m, MonadIO m) => FilePath -> String ->  m String
downloadURL location url = do
  logFunction <- asks envLog
  request <- liftIO $ parseUrlThrow url
  liftIO $ runResourceT
         $ runConduit $  httpSource request (processResponse logFunction)
         .| sinkFile location
  return location
  where
     processResponse logFunction response = do
         _ <- liftIO $ logFunction $ "Downloading " ++ "[" ++ formatSize response ++ "] " ++ url
         getResponseBody response
     formatSize response = maybe "unknown" showAsMB (listToMaybe (getResponseHeader "Content-Length" response) >>= stringToInt . BC.unpack)


stringToInt :: String -> Maybe Int
stringToInt cs =  case reads cs :: [(Int,String)] of
                      [(n, _)] -> Just n
                      _        -> Nothing


