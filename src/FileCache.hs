{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCache where
import           Control.Concurrent.Async (Async, async)
import           Control.Concurrent.MVar  (modifyMVar, modifyMVar_, newMVar,
                                           withMVar)
import           Control.Exception.Safe   (catch, onException, throwIO)
import           Control.Monad            (filterM)
import           Control.Monad.Reader     (ask, asks, liftIO, runReaderT)
import           Data.List                (isSuffixOf)
import           Data.Map.Strict          as M (Map, delete, empty, fromList,
                                                insert, keys, lookup)
import           Env                      (App, Cache (..), Env (..))
import           Files                    (downloadURL)
import           Log
import           System.Directory         (getDirectoryContents, removeFile)
import           System.IO.Error          (isDoesNotExistError)
type Key = String
type Value = String

empty :: IO (Cache k v)
empty = cache M.empty

cache :: M.Map k (Async v) -> IO (Cache k v)
cache m = Cache <$> newMVar m

keys :: Cache k v -> IO [k]
keys (Cache mvar) = withMVar mvar $ return . M.keys


fromFile :: FilePath -> IO (Cache String String)
fromFile dir = do
  content <-  getDirectoryContents dir >>= filterM (return . isSuffixOf ".zip")
  asyncs <- mapM (async . return) content
  cache $ M.fromList $ zip content asyncs

getOrCompute :: Key ->  App Value -> App (Async Value)
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


getFilePath :: Key -> String-> App (Async Value)
getFilePath path url = getOrCompute path download
  where
     download:: App String
     download =  downloadURL path url `onException` cleanup path
     cleanup:: String -> App ()
     cleanup file = do
          say $ "cleanup: deleting file " ++ file
          liftIO $ removeIfExists file


removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e


removeEntry :: Key -> App ()
removeEntry key = do
  (Cache cacheMVar) <- asks envCache
  liftIO $ modifyMVar_ cacheMVar (return . M.delete key)
  return ()
