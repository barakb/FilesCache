{-# LANGUAGE ScopedTypeVariables #-}

module FileCache where
import           Control.Concurrent.Async (Async, async)
import           Control.Concurrent.MVar  (modifyMVar, modifyMVar_, newMVar,
                                           withMVar)
import           Control.Exception.Safe   (catch, onException, throwIO)
import           Control.Monad            (filterM)
import           Control.Monad.Reader     (ask, asks, liftIO, runReaderT)
import           Data.List                (isSuffixOf)
import           Data.Map.Strict          as M (delete, keys)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import           Env                      (App, Env (..), LRUCache (..),
                                           ProtectedCache (..))
import           Files                    (downloadURL)
import           Log
import           LRUCache                 as LRU
import           System.Directory         (getDirectoryContents, removeFile)
import           System.IO.Error          (isDoesNotExistError)

type Key = T.Text
type Value = T.Text

keys :: ProtectedCache k v -> IO [k]
keys pc = withMVar (pMvar pc) $ return . M.keys . cCache


fromDirectory :: Int -> FilePath -> IO (ProtectedCache T.Text T.Text)
fromDirectory capacity dir = do
  content <-  getDirectoryContents dir
  validContent <- filterM (return . Data.List.isSuffixOf ".zip") content
  lruCache <- LRU.fromList capacity (map T.pack validContent)
  withLock <- newMVar lruCache
  return $ ProtectedCache withLock

getOrCompute :: Key ->  App Value -> App ([Key], Async Value)
getOrCompute key compute =  do
   (ProtectedCache cacheMVar) <- asks envCache
   env <- ask
   liftIO $ modifyMVar cacheMVar (getOrUpdate env)
   where
     getOrUpdate :: Env -> LRUCache Key Value -> IO (LRUCache Key Value, ([Key], Async Value))
     getOrUpdate env lruCache = case LRU.get key lruCache of
        Just (asyncValue, modifiedCache) -> return (modifiedCache, ([], asyncValue))
        Nothing -> do
          value <- async $ runReaderT compute env
          let(keysToRemove, modifiedCache) = LRU.adjustSize (LRU.insert key value lruCache)
          return (modifiedCache, (keysToRemove, value))


getFilePath :: Key -> T.Text-> App ([Key], Async Value)
getFilePath path url = getOrCompute path download
  where
     download:: App T.Text
     download =  downloadURL path url `onException` cleanup path
     cleanup:: T.Text -> App ()
     cleanup file = do
          say $ "cleanup: deleting file " <> file
          liftIO $ removeIfExists file


removeIfExists :: T.Text -> IO ()
removeIfExists fileName = removeFile (T.unpack fileName) `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e


removeEntry :: Key -> App ()
removeEntry key = do
  (ProtectedCache cacheMVar) <- asks envCache
  liftIO $ modifyMVar_ cacheMVar (return . cacheInfoDeleteKey key)
  return ()

cacheInfoDeleteKey :: Ord k => k -> LRUCache k v -> LRUCache k v
cacheInfoDeleteKey k ci = ci{cCache = M.delete k (cCache ci)}
