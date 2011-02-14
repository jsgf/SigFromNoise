{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Trans
import Control.Applicative ((<$>), (<*>), (<|>), empty, pure)

import qualified Data.Attoparsec.Enumerator as AE

import qualified Data.Aeson as Aeson

import qualified Data.Enumerator as DE

import qualified Network.HTTP.Enumerator as HE
import qualified Network.Wai as W

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base64 as B64
import qualified Text.Show.ByteString as BS
import qualified Network.Riak as R
import qualified Network.Riak.Types as R

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.Twitter.Types

basicauth :: String -> String -> HE.Request -> HE.Request
basicauth u p r = r { HE.requestHeaders = basicauthhdr u p : HE.requestHeaders r }

basicauthhdr u p = (W.mkCIByteString "authorization", C8.concat [ "Basic ", auth ])
    where auth = B64.encode $ C8.concat $ map C8.pack [ u, ":",  p ]

stashTweet c t = R.put c "Tweets" key Nothing t R.One R.One
    where key = LC8.pack . show . twitterid . t_id $ t

httpiter conn st _ | st == W.status200 = go
                   | otherwise = DE.throwError $ HE.StatusCodeException (W.statusCode st) (LBS.fromChunks [W.statusMessage st])
    where
      go = do
        eof <- DE.isEOF
        if eof then return ()
          else do
            x <- AE.iterParser Aeson.json
            case Aeson.fromJSON x :: Aeson.Result Tweet of
              Aeson.Success a -> liftIO $ do
                                   -- putStrLn $ show a
                                   putStrLn $ "Stashing tweet " ++
                                          (show . twitterid . t_id $ a) ++ " (" ++
                                          (T.unpack . tu_name . t_user $ a) ++ "): " ++
                                          (T.unpack . t_text $ a)
                                   stashTweet conn a
                                   return ()
              Aeson.Error e -> liftIO $ putStrLn $ "Failed: " ++ show x ++ " -> " ++ e
        go

main = do
  r_conn <- R.connect $ R.Client "127.0.0.1" "8081" LBS.empty
  request <- basicauth "SignalsFromNois" "R4sytW7XHs" <$> HE.parseUrl "http://stream.twitter.com/1/statuses/sample.json" 
  DE.run_ $ HE.httpRedirect request (httpiter r_conn)
