{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Concurrent.Async  (waitCatch)
import           Control.Monad.Reader      (liftIO, runReaderT)
import           Data.List                 (intercalate)
import           Data.Text                 as T (unpack)
import           Env                       (App, Env (..))
import           FileCache                 (fromDirectory, getFilePath, keys,
                                            removeEntry, removeIfExists)
import           Log                       (createLogF, say, sayContent,
                                            timeCommand)
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
  cache <- FileCache.fromDirectory 1 "."
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
           (filesToDelete, asyn) <- getFilePath fileName fullPath
           removeFiles filesToDelete
           maybeFile <- timeCommand ("download of file [" ++ fileName ++ "]") (liftIO $ waitCatch asyn)
           case maybeFile of
              Right localPath ->
                timeCommand ("Serving file: " ++ localPath) $ liftIO $ respond $ responseFile status200 [(hContentType, "text/plain")] localPath Nothing
              Left err -> do
                say $ "Error while trying to retrieve " ++ fullPath ++ " " ++ show err
                removeEntry fileName
                liftIO $ respond $ responseLBS notFound404 [(hContentType, "text/plain")] "Failed to bring file to cache"
        Nothing -> liftIO $ respond $ responseLBS notFound404 [(hContentType, "text/plain")] "Not Found"



getRequestedFileName :: Request -> Maybe (String, String)
getRequestedFileName req = let pathInfo' =  T.unpack <$> pathInfo req
                               method = requestMethod req
                            in
                               if method == "GET" && 1 < length pathInfo' && takeExtension (last pathInfo') == ".zip"
                               then
                                  Just ("http://" ++ intercalate "/" (drop 1 pathInfo'), last pathInfo')
                               else
                                  Nothing

removeFiles :: [String] -> App ()
removeFiles [] = return ()
removeFiles files = do
  sayContent $ "Removing files " ++ show files ++ ", cache is: "
  liftIO $ mapM_ removeIfExists files
  return ()

-- http://localhost:3000/file/hercules/12.3.1/master/19209-63/gigaspaces-xap-enterprise-12.3.1-rc2-b19209-63.zip
