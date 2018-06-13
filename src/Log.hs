{-# LANGUAGE FlexibleContexts #-}
module Log where
import Control.Concurrent.MVar  (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (bracket)
import Control.Monad.Reader
import Env
import Prelude


logf :: MVar Bool -> String -> IO ()
logf mvar msg = bracket lock unloack logMessage
  where lock = takeMVar mvar
        unloack = const (putMVar mvar True)
        logMessage = const (putStrLn msg)


log :: (MonadReader Env m, MonadIO m) => String -> m()
log msg = do
  env <- ask
  liftIO $ envLog env  msg

createLogF :: IO (String -> IO ())
createLogF = do
  logMvar <- newMVar True
  return $ logf logMvar

{-
run :: IO ()
run = do
  logMvar <- newMVar True
  let env = Env
          {envLog = logf logMvar}
  runReaderT (log "foo") env
-}


