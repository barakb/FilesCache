{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module FileCache where
import           Control.Concurrent.Async (Async, async)
import           Control.Concurrent.MVar  (modifyMVar, newMVar, readMVar)
import           Control.Exception.Safe   (catch, onException, throwIO)
import           Data.Map.Strict          as M (Map, empty, insert, lookup, keys)
import           Files                    (downloadURL)
import           Network.URI              (parseURI, uriPath)
import           System.Directory         (removeFile)
import           System.FilePath.Posix    (takeFileName)
import           System.IO.Error          (isDoesNotExistError)
import           System.Posix             (FileOffset, fileSize, getFileStatus)
import           Env (Cache(..), Env(..))
import           Control.Monad.Reader(MonadReader, MonadIO, liftIO, asks)




empty :: IO (Cache k v)
empty = Cache <$> newMVar M.empty

printContent ::  (Show k, Show v) => String -> Cache k v -> IO ()
printContent prompt (Cache mvar) = readMVar mvar >>= (\m -> putStrLn $ prompt ++ ":" ++ show (keys m))

get :: forall k v . (Ord k, Show k, Show v) => k -> Cache k v -> IO (Maybe v) -> IO (Async (Maybe v))
get key (Cache mvar) compute = modifyMVar mvar getOrUpdate
  where
    getOrUpdate :: M.Map k  (Async (Maybe v)) -> IO(M.Map k  (Async (Maybe v)), Async (Maybe v))
    getOrUpdate m = do
      putStrLn $ "searching for " ++ show key ++ " keys in maps are " ++ show (M.keys m)
      case M.lookup key m of
        Just value -> putStrLn ("resource " ++ show key ++ " found in, waiting on computation") >> return (m, value)
        Nothing -> do
                     putStrLn $ "resource " ++ show key ++ " not found, recomputing"
                     value <- async compute
                     return (M.insert key value m, value)


getFilePath :: (MonadReader Env m, MonadIO m) => String -> m (Async (Maybe String))
getFilePath url = do
  cache <- asks envCache
  liftIO $ get url cache download
  where
  download:: IO (Maybe String)
  download = case urlToLocalPath url of
                  Just p ->  downloadFile p `onException` cleanup p
                  Nothing -> return Nothing
  downloadFile:: String -> IO (Maybe String)
  downloadFile file = do
                      _ <- downloadURL url file
                      return $ Just file
  cleanup:: String -> IO ()
  cleanup file = do
    putStrLn $ "cleanup: deleting file " ++ file
    removeIfExists file
    return ()



urlToLocalPath :: String -> Maybe String
urlToLocalPath url = takeFileName . uriPath <$> parseURI  url

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e



