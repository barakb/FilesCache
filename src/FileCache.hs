module FileCache where
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Exception.Safe
import           Data.Map.Strict          as M hiding (drop)
import           Files                    (downloadURL)
import           Network.URI
import           System.Directory
import           System.FilePath.Posix    (takeFileName)
import           System.IO.Error          (isDoesNotExistError)

newtype Cache k v = Cache (MVar (M.Map k  (Async (Maybe v))))

empty :: IO (Cache k v)
empty = Cache <$> newMVar M.empty

get :: Ord k => k -> Cache k v -> IO (Maybe v) -> IO (Async (Maybe v))
get key (Cache mvar) compute = modifyMVar mvar getOrUpdate
  where
    -- getOrUpdate :: (M.Map k  (Async (Maybe v))) -> IO(M.Map k  (Async (Maybe v)), (Async (Maybe v)))
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
                  Just p -> do
                      _<- downloadURL url p  `onException` cleanup p
                      return $ Just p
                  Nothing -> return Nothing
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

url1::String
url1 = "http://hercules/12.3.1/master/19208-19/gigaspaces-xap-enterprise-12.3.1-m9-b19208-19.zip"

dl = do
  cache <- FileCache.empty
  asyn <- getFilePath "http://hercules/12.3.1/master/19208-19/gigaspaces-xap-enterprise-12.3.1-m9-b19208-19.zip" cache
  wait asyn
  putStrLn " Done with the first file"
  asyn1 <- getFilePath "http://hercules/12.3.1/master/19208-19/gigaspaces-xap-enterprise-12.3.1-m9-b19208-19.zip" cache
  putStrLn " Done with the second file"
  wait asyn1

