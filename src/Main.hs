module Main where


import           Control.Concurrent.Async  (waitCatch)
import           Control.Monad.Reader      (asks, liftIO, runReaderT)
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import           Env                       (App, Config (..), Env (..),
                                            cacheDirPrefix)
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
import           Options.Applicative
import           System.Environment        (getProgName)
import           System.FilePath.Posix     (takeExtension)


main :: IO ()
main = do
  progName <- getProgName
  execParser (opts progName) >>= runWithConfig
  where opts progName = info (parseConfig <**> helper)
                     ( fullDesc
                    <> header (progName <> "- a program for serving cached large files over http"))


parseConfig :: Parser Config
parseConfig = Config <$> option auto
                          (long "size"
                          <> short 's'
                          <> help "the size of the cache"
                          <> showDefault
                          <> value 200
                          <> metavar "INT")
                     <*> strOption
                           (long "dir"
                           <> short 'd'
                           <> help "the location of cache dir on disk"
                           <> showDefault
                           <> value ".")


runWithConfig :: Config -> IO ()
runWithConfig conf = do
  let port = 3000
  cache <- FileCache.fromDirectory (cacheSize conf) (cacheDir conf)
  logFunc <- createLogF
  itemsInCache <- FileCache.keys cache
  logFunc $ "Config is " <> asText conf
  logFunc $ "Building cache of size " <> asText (cacheSize conf) <> " from " <> asText itemsInCache
  logFunc $ "Listening on port " <> asText port
  let env = Env{envLog = logFunc, envCache=cache, config=conf}
  run port $ app env


asText :: (Show a)  => a -> T.Text
asText = T.pack . show

app :: Env -> Application
app env req respond  =  runReaderT (runWithEnv req respond) env

runWithEnv :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
runWithEnv request respond = do
    conf <- asks config
    case getRequestedFileName request (cacheDirPrefix conf) of
        Just (fullPath, fileName) -> do
           say $ "requested: " <> fileName <> " [" <> fullPath <> "]"
           (filesToDelete, asyn) <- getFilePath fileName fullPath
           removeFiles filesToDelete
           maybeFile <- timeCommand ("download of file [" <> fileName <> "]") (liftIO $ waitCatch asyn)
           case maybeFile of
              Right localPath ->
                timeCommand ("Serving file: " <> localPath) $ liftIO $ respond $ responseFile status200 [(hContentType, "text/plain")] (T.unpack localPath) Nothing
              Left err -> do
                say $ "Error while trying to retrieve " <> fullPath <> " " <> asText err
                removeEntry fileName
                liftIO $ respond $ responseLBS notFound404 [(hContentType, "text/plain")] "Failed to bring file to cache"
        Nothing -> liftIO $ respond $ responseLBS notFound404 [(hContentType, "text/plain")] "Not Found"



takeExtension' :: T.Text -> T.Text
takeExtension' = T.pack . takeExtension . T.unpack

getRequestedFileName :: Request -> T.Text -> Maybe (T.Text, T.Text)
getRequestedFileName req prefix = let pathInfo' =  pathInfo req
                                      method = requestMethod req
                                  in
                                     if method == "GET" && 1 < length pathInfo' && takeExtension' (last pathInfo') == ".zip"
                                     then
                                        Just ("http://" <> T.intercalate "/" (drop 1 pathInfo'), prefix <> last pathInfo')
                                     else
                                        Nothing

removeFiles :: [T.Text] -> App ()
removeFiles [] = return ()
removeFiles files = do
  sayContent $ "Removing files " <> asText files <> ", cache is: "
  conf <- asks config
  let prefix = cacheDirPrefix conf
      filesWithPrefix = (prefix <>) <$> files
  liftIO $ mapM_ removeIfExists filesWithPrefix
  return ()

-- http://localhost:3000/file/hercules/12.3.1/master/19209-63/gigaspaces-xap-enterprise-12.3.1-rc2-b19209-63.zip
