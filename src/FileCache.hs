{-# LANGUAGE ScopedTypeVariables #-}
module FileCache where
import           Control.Concurrent.Async (Async, async, wait)
import           Control.Concurrent.MVar  (MVar, modifyMVar, newMVar)
import           Control.Exception.Safe   (catch, onException, throwIO)
import           Control.Monad            (when)
import           Data.Map.Strict          as M (Map, empty, insert, lookup)
import           Files                    (downloadURL)
import           Network.URI              (parseURI, uriPath)
import           System.Directory         (doesFileExist, removeFile)
import           System.FilePath.Posix    (takeFileName)
import           System.IO.Error          (isDoesNotExistError)
import           System.Posix             (FileOffset, fileSize, getFileStatus)

newtype Cache k v = Cache (MVar (M.Map k  (Async (Maybe v))))

empty :: IO (Cache k v)
empty = Cache <$> newMVar M.empty

get :: forall k v . Ord k => k -> Cache k v -> IO (Maybe v) -> IO (Async (Maybe v))
get key (Cache mvar) compute = modifyMVar mvar getOrUpdate
  where
    getOrUpdate :: M.Map k  (Async (Maybe v)) -> IO(M.Map k  (Async (Maybe v)), Async (Maybe v))
    getOrUpdate m = case M.lookup key m of
      Just value -> return (m, value)
      Nothing -> do
                   value <- async compute
                   return (M.insert key value m, value)

getFilePath :: String -> Cache String String  -> IO (Async (Maybe String))
getFilePath url cache = get url cache download
  where
  download:: IO (Maybe String)
  download = case urlToLocalPath url of
                  Just p ->  downloadFile p `onException` cleanup p
                  Nothing -> return Nothing
  downloadFile:: String -> IO (Maybe String)
  downloadFile file = do
                      _ <- downloadURL url file
                      fileShouldBeDeleted <- andM (doesFileExist file) (isFileEmpty file)
                      when fileShouldBeDeleted $ removeIfExists file
                      fileExists <- doesFileExist file
                      if fileExists then
                          return $ Just file
                      else
                         return Nothing

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

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

andM :: Monad m => m Bool -> m Bool -> m Bool
andM a b = do
    a' <- a
    b' <- b
    return (a' && b')

isFileEmpty :: String -> IO Bool
isFileEmpty path = (==0) <$> getFileSize path

url1::String
url1 = "http://hercules/12.3.1/master/19208-19/gigaspaces-xap-enterprise-12.3.1-m9-b19208-19.zip"

dl:: IO(Maybe String)
dl = do
  cache <- FileCache.empty
  asyn <- getFilePath "http://hercules/12.3.1/master/19208-19/gigaspaces-xap-enterprise-12.3.1-m9-b19208-19.zip1" cache
  _ <- wait asyn
  putStrLn " Done with the first file"
  asyn1 <- getFilePath "http://hercules/12.3.1/master/19208-19/gigaspaces-xap-enterprise-12.3.1-m9-b19208-19.zip" cache
  putStrLn " Done with the second file"
  wait asyn1

