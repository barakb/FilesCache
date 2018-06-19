{-# LANGUAGE FlexibleContexts #-}
module Log where
import Control.Concurrent.MVar  (MVar, newMVar, takeMVar, putMVar, readMVar)
import Control.Exception (bracket)
import Control.Monad.Reader
import Env(ProtectedCache(..), App(), Env(..))
import Control.Concurrent (myThreadId)
import Data.Time.Clock(getCurrentTime)
import Data.Time.Format(formatTime, defaultTimeLocale)
import           Criterion.Measurement(getTime, secs)


logf :: MVar Bool -> String -> IO ()
logf mvar msg = bracket lock unloack logMessage
  where lock = takeMVar mvar
        unloack = const (putMVar mvar True)
        logMessage = const (putStrLn msg)


say :: (MonadReader Env m, MonadIO m) => String -> m()
say msg = do
  env <- ask
  liftIO $ envLog env  msg

createLogF :: IO (String -> IO ())
createLogF = do
  logMvar <- newMVar True
  return  (\msg -> do
              thread <- myThreadId
              time <- liftIO getCurrentTime
              logf logMvar $ "[" ++  formatTime defaultTimeLocale "%H:%M:%S%Q %z"  time ++ "][" ++ show thread ++ "] -> " ++ msg)

timeCommand :: String -> App a -> App a
timeCommand prompt action = do
    start <- liftIO  getTime
    action' <- action
    end <- liftIO getTime
    say $ prompt ++ " took " ++ secs (end - start)
    return action'


sayContent :: String -> App ()
sayContent prompt = do
  (ProtectedCache cacheMVar) <- asks envCache
  m <- liftIO $ readMVar cacheMVar
  say $ prompt ++ ":" ++ show m

