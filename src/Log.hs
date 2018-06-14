{-# LANGUAGE FlexibleContexts #-}
module Log where
import Control.Concurrent.MVar  (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (bracket)
import Control.Monad.Reader
import Env
import Control.Concurrent (myThreadId)
import Data.Time.Clock(getCurrentTime)
import Data.Time.Format(formatTime, defaultTimeLocale)

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

{-
run :: IO ()
run = do
  logMvar <- newMVar True
  let env = Env
          {envLog = logf logMvar}
  runReaderT (log "foo") env
-}


