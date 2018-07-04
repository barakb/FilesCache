module LRUCache(LRUCache.empty, LRUCache.fromList, LRUCache.get, LRUCache.insert, LRUCache.adjustSize) where

import           Control.Concurrent.Async (Async, async)
import           Data.Foldable            (foldl')
import           Data.Map.Strict          as M (assocs, delete, empty, fromList,
                                                insert, lookup)

import           Data.Sort                (sortOn)
import           Data.Text
import           Env

empty :: Int ->  LRUCache k v
empty capacity = LRUCache capacity 0 0 M.empty

fromList :: Int -> [Text] -> IO (LRUCache Text Text)
fromList capacity list = do
  asyncs <- mapM (async . return) list
  let size = Prelude.length list
      values = Prelude.zipWith Value asyncs [0 .. ]
      m = M.fromList $ Prelude.zip list values
  return $ LRUCache capacity size (fromIntegral size) m


get :: Ord k => k -> LRUCache k v -> Maybe (Async v, LRUCache k v)
get key cache = do
  found <- M.lookup key (cCache cache)
  let modifiedValue = found{lmt = cTick cache + 1}
      modifiedCache = M.insert key modifiedValue (cCache cache)
      ret = cache{cCache = modifiedCache, cTick = cTick cache + 1}
  return (val modifiedValue, ret)

insert :: Ord k => k -> Async v -> LRUCache k v -> LRUCache k v
insert key async' cache = let value' = Value async' (cTick cache)
                              modifiedMap = M.insert key value' (cCache cache)
                              modifiedTick = cTick cache + 1
                              modifiedSize = cSize cache + 1
                          in
                              cache{cSize = modifiedSize, cTick = modifiedTick, cCache = modifiedMap}

delete :: Ord k => k -> LRUCache k v -> LRUCache k v
delete key cache = case M.lookup key (cCache cache) of
                  Nothing -> cache
                  Just _ -> let modifiedMap = M.delete key (cCache cache)
                                modifiedSize = cSize cache - 1
                            in
                                cache{cSize = modifiedSize, cCache = modifiedMap}



adjustSize :: (Ord k, Show k) => LRUCache k v -> ([k], LRUCache k v)
adjustSize c@(LRUCache capacity size  _ cache) =
  if size <= capacity
  then ([], c)
  else let
           sortedEntries = sortOn (lmt . snd) $ M.assocs cache
           keysToRemove = Prelude.take(size - capacity) $ fst <$> sortedEntries

       in
            (keysToRemove, Data.Foldable.foldl' (flip LRUCache.delete) c keysToRemove)
