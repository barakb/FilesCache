{-# LANGUAGE FlexibleContexts #-}
module Log where
import Control.Concurrent.MVar (readMVar)
import Control.Monad.Reader
import Env(ProtectedCache(..), App(), Env(..))
import Control.Concurrent (myThreadId)
import Data.Time.Clock(getCurrentTime)
import Data.Time.Format(formatTime, defaultTimeLocale)
import Criterion.Measurement(getTime, secs)
import qualified Data.ByteString as BS (hPutStrLn)
import Data.Text.Encoding (encodeUtf8)
import System.IO (stdout)
import Data.Text
import           Data.Monoid((<>))


-- https://www.snoyman.com/blog/2016/11/haskells-missing-concurrency-basics


say :: (MonadReader Env m, MonadIO m) => Text -> m()
say msg = do
  env <- ask
  liftIO $ envLog env msg

createLogF :: IO (Text -> IO ())
createLogF =  return  (\msg -> do
                            thread <- myThreadId
                            time <- liftIO getCurrentTime
                            say' $ Data.Text.concat [ "[" , pack $ formatTime defaultTimeLocale "%H:%M:%S%Q %z"  time ,"][" ,pack $ show thread ,"] -> " ,msg])

timeCommand :: Text -> App a -> App a
timeCommand prompt action = do
    start <- liftIO  getTime
    action' <- action
    end <- liftIO getTime
    say $ mconcat [prompt , " took " , pack $ secs (end - start)]
    return action'


sayContent :: Text -> App ()
sayContent prompt = do
  (ProtectedCache cacheMVar) <- asks envCache
  m <- liftIO $ readMVar cacheMVar
  say $ prompt <> ":" <> pack (show m)


say' :: Text -> IO ()
say' = BS.hPutStrLn stdout . encodeUtf8

print' :: (Show a) => a -> IO()
print' =  say' . pack. show
