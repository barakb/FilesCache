{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Concurrent.Async  (wait)
import           Control.Monad.Reader      (ReaderT, liftIO, runReaderT)
import           Criterion.Measurement     (getTime, secs)
import           Data.List                 (intercalate)
import           Data.Text                 as T (unpack)
import           Env                       (Env (..))
import           FileCache                 (empty, getFilePath)
import           Log                       (createLogF, log)
import           Network.HTTP.Types        (notFound404, status200)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai               (Application, Request (..), Response,
                                            ResponseReceived, responseFile,
                                            responseLBS)
import           Network.Wai.Handler.Warp  (run)
import           Prelude                   hiding (log)
import           System.FilePath.Posix     (takeExtension)



main :: IO ()
main = do
  let port = 3000
  cache <- FileCache.empty
  logFunc <- createLogF
  logFunc $ "Listening on port " ++ show port
  let env = Env{envLog = logFunc, envCache=cache}
  run port $ app env

app :: Env -> Application
app env req respond  =  runReaderT (runWithEnv req respond) env

runWithEnv :: Request -> (Response -> IO ResponseReceived) -> ReaderT Env IO ResponseReceived
runWithEnv request respond =
    case getRequestedFileName request of
        Just (fullPath, fileName) -> do
           log $ "request for: " ++ fileName ++ " full path is " ++ fullPath
           asyn <- getFilePath fullPath
           (t , maybeFile) <- liftIO $ timeIt $ wait asyn
           log $ fullPath ++ " took " ++ secs t
           case maybeFile of
              Just localPath -> liftIO $ respond $ responseFile status200 [(hContentType, "text/plain")] localPath Nothing
              Nothing -> liftIO $ respond $ responseLBS notFound404 [(hContentType, "text/plain")] "Failed to bring file to cache"
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

timeIt :: IO a -> IO (Double, a)
timeIt action = do
    start <- getTime
    a <- action
    end <- getTime
    return (end - start, a)
