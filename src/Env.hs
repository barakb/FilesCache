module Env where
import           Control.Concurrent.Async (Async)
import           Control.Concurrent.MVar  (MVar)
import           Control.Monad.Reader     (ReaderT)
import           Data.Int                 (Int64)
import           Data.Map.Strict          as M (Map, elems, keys)
import           Data.Monoid              ((<>))
import           Data.Text                as T hiding (zip)


type App = ReaderT Env IO

data Env = Env{
  envLog   :: !(Text -> IO ()),
  envCache :: !(ProtectedCache Text Text),
  config   :: !Config
}

data Config = Config {
  cacheSize :: Int,
  cacheDir  :: String
} deriving Show


cacheDirPrefix :: Config -> Text
cacheDirPrefix conf = (T.pack . cacheDir) conf <> "/"

type Priority = Int64

data Value v = Value {val :: Async v, lmt :: !Priority}
instance Show (Value v) where
  show value = "Value [lmt=" ++ show (lmt value) ++ "]"

data LRUCache k v = LRUCache {
  cCapacity :: !Int,
  cSize     :: !Int,
  cTick     :: !Priority,
  cCache    :: !(M.Map k (Value v))
}

instance Show k => Show (LRUCache k v)  where
  show (LRUCache capacity size tick cache) = "LRUCache [cCapacity=" ++ show capacity ++ ", cSize=" ++ show size ++ ", cTick=" ++ show tick ++
      ", elements=(" ++ show (zip (M.keys cache) (lmt <$> M.elems cache)) ++")]"

newtype ProtectedCache k v = ProtectedCache {pMvar :: MVar (LRUCache k v)}


{-
cache <- FileCache.fromDirectory 1 "."
:m +Control.Concurrent.MVar
c <- withMVar (pMvar cache) return
:m +Data.Map.Strict
:m +Data.Sort
sortOn (lmt . snd) $ assocs (cCache c)
-}
