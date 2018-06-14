{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Concurrent.Async  (waitCatch)
import           Control.Monad.Reader      (liftIO, runReaderT)
import           Data.List                 (intercalate)
import           Data.Text                 as T (unpack)
import           Env                       (App, Env (..))
import           FileCache                 (fromFile, getFilePath, keys,
                                            removeEntry)
import           Log                       (createLogF, say, timeCommand)
import           Network.HTTP.Types        (notFound404, status200)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai               (Application, Request (..), Response,
                                            ResponseReceived, responseFile,
                                            responseLBS)
import           Network.Wai.Handler.Warp  (run)
import           System.FilePath.Posix     (takeExtension)



main :: IO ()
main = do
  let port = 3000
  cache <- FileCache.fromFile "."
  logFunc <- createLogF
  itemsInCache <- FileCache.keys cache
  logFunc $ "Building cache from " ++ show itemsInCache
  logFunc $ "Listening on port " ++ show port
  let env = Env{envLog = logFunc, envCache=cache}
  run port $ app env

app :: Env -> Application
app env req respond  =  runReaderT (runWithEnv req respond) env

runWithEnv :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
runWithEnv request respond =
    case getRequestedFileName request of
        Just (fullPath, fileName) -> do
           say $ "requested: " ++ fileName ++ " [" ++ fullPath ++ "]"
           asyn <- getFilePath fileName fullPath
           maybeFile <- timeCommand ("download of file [" ++ fileName ++ "]") (liftIO $ waitCatch asyn)
           case maybeFile of
              Right localPath ->
                timeCommand ("Serving file: " ++ localPath) $ liftIO $ respond $ responseFile status200 [(hContentType, "text/plain")] localPath Nothing
              Left err -> do
               say $ "Error while trying to retrieve " ++ fullPath ++ " " ++ show err
               removeEntry fileName
               liftIO $ respond $ responseLBS notFound404 [(hContentType, "text/plain")] "Failed to bring file to cache"
        Nothing -> liftIO $ respond $ responseLBS notFound404 [(hContentType, "text/plain")] "Not Found"

--        ["file","gigaspaces-xap-enterprise-12.3.1-m9-b19208-19.zip"]


getRequestedFileName :: Request -> Maybe (String, String)
getRequestedFileName req = let pathInfo' =  T.unpack <$> pathInfo req
                               method = requestMethod req
                            in
                               if method == "GET" && 1 < length pathInfo' && takeExtension (last pathInfo') == ".zip"
                               then
                                  Just ("http://" ++ intercalate "/" (drop 1 pathInfo'), last pathInfo')
                               else
                                  Nothing

