module Env where
import           Control.Concurrent.MVar  (MVar)
import           Data.Map.Strict          as M (Map)
import           Control.Concurrent.Async (Async)


data Env = Env{
  envLog :: !(String -> IO ()),
  envCache :: !(Cache String String)
}

newtype Cache k v = Cache (MVar (M.Map k  (Async (Maybe v))))


