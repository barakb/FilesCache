
{-# LANGUAGE OverloadedStrings #-}
module Main where


import           FileCache
import           Files
import           Network.HTTP.Types        (status200)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai               (responseLBS)
import           Network.Wai.Handler.Warp  (run)



main :: IO ()
main = putStrLn "dfdas" -- copy "/home/barakbo/tmp/nvme.html" "/home/barakbo/haskell/FilesCache/nvme.html"

{-
http://hercules/12.3.1/master/19208-19/gigaspaces-xap-enterprise-12.3.1-m9-b19208-19.zip

main = run 3000 $ \_ f -> f $
    responseLBS status200 [(hContentType, "text/plain")] "Hello, world!\n"
-}


{-

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Blaze.ByteString.Builder
import Data.ByteString.Char8 (pack)
import Data.FileEmbed
import Data.List (isSuffixOf)
import Data.Map (fromList, (!), member)
import Network.Wai
import Network.Wai.Handler.Warp (runEnv)
import Network.HTTP.Types
import Network.HTTP.Types.Header

m = let
  m0 = map (\(k, v) -> (('/':k), v)) $(embedDir "www")
  m1 = [(take (length k - 10) k , v)
    | (k, v) <- m0, "/index.html" `isSuffixOf` k]
  in fromList $ map (\(k, v) -> (pack k, v)) (m0 ++ m1)

main = do
  runEnv 3000 $ \req f ->
    case requestMethod req of
    "GET" -> do
      putStrLn $ show (remoteHost req) ++ " " ++ show (rawPathInfo req)
      let k = rawPathInfo req in f $
        if k `member` m then
          responseBuilder status200 [] $ fromByteString (m!k)
        else
          responseBuilder status301 [(hLocation, "/")] $
            fromByteString "Redirect to /"
-}
