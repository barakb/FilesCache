{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCache where
import           Control.Concurrent.Async (Async, async)
import           Control.Concurrent.MVar  (modifyMVar, newMVar, readMVar)
import           Control.Exception        (Exception)
import           Control.Exception.Safe   (catch, onException, throw, throwIO)
import           Control.Monad.Reader     (ask, asks, liftIO, runReaderT)
import           Data.Map.Strict          as M (Map, empty, insert, keys,
                                                lookup)
import           Data.Maybe               (maybe)
import           Env                      (App, Cache (..), Env (..))
import           Files                    (downloadURL)
import           Log
import           Network.URI              (parseURI, uriPath)
import           System.Directory         (removeFile)
import           System.FilePath.Posix    (takeFileName)
import           System.IO.Error          (isDoesNotExistError)

type Key = String
type Value = String

empty :: IO (Cache k v)
empty = Cache <$> newMVar M.empty

printContent ::  (Show k, Show v) => String -> Cache k v -> IO ()
printContent prompt (Cache mvar) = readMVar mvar >>= (\m -> putStrLn $ prompt ++ ":" ++ show (keys m))

getOrCompute :: Key -> App Value -> App (Async Value)
getOrCompute key compute = do
  (Cache cacheMVar) <- asks envCache
  env <- ask
  liftIO $ modifyMVar cacheMVar (getOrUpdate env)
  where
    getOrUpdate :: Env -> Map Key (Async Value) -> IO (Map Key (Async Value), Async Value)
    getOrUpdate env m = case M.lookup key m of
                      Just value -> return (m, value)
                      Nothing -> do
                          value <- async $ runReaderT compute env
                          return (M.insert key value m, value)


getFilePath :: Key -> App (Async Value)
getFilePath url = getOrCompute url download
  where
  download:: App String
  download =  downloadURL url path `onException` cleanup path
              where path = urlToLocalPath url
  cleanup:: String -> App ()
  cleanup file = do
    say $ "cleanup: deleting file " ++ file
    liftIO $ removeIfExists file


newtype ParseURLException = ParseURLException String
    deriving (Show,  Eq)

instance Exception ParseURLException

urlToLocalPath :: String -> String
urlToLocalPath url = maybe (throw $ ParseURLException url) (takeFileName . uriPath) (parseURI  url)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e



