{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs)

import Control.Monad
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Loops (whileM_)
import Control.Applicative ((<$>), (<*>), (<|>), empty, pure)
import Control.Arrow (first, second, (***))
import Data.Monoid (Monoid(..))

import qualified Data.Attoparsec.Enumerator as AE

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.:), (.=))

import qualified Data.Enumerator as DE

import Network (withSocketsDo)
import qualified Network.HTTP.Enumerator as HE
import qualified Network.Wai as W

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base64 as B64
import qualified Network.Riak as R
import qualified Network.Riak.Types as R

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.List

import Network.Twitter.Types

import qualified Network.OAuth.Consumer as O
import qualified Network.OAuth.Http.HttpClient as O
import qualified Network.OAuth.Http.Request as O
import qualified Network.OAuth.Http.Response as O

basicauth :: String -> String -> HE.Request -> HE.Request
basicauth u p r = r { HE.requestHeaders = basicauthhdr u p : HE.requestHeaders r }

basicauthhdr u p = (W.mkCIByteString "authorization", C8.concat [ "Basic ", auth ])
    where auth = B64.encode $ C8.concat $ map C8.pack [ u, ":",  p ]

stashTweet c t = R.put c "Tweets" key Nothing t R.One R.One
    where key = LC8.pack . show . twitterid . t_id $ t

untilDone = whileM_ $ liftM not DE.isEOF

httpiter conn st _ | st == W.status200 = untilDone go
                   | otherwise = DE.throwError $ HE.StatusCodeException (W.statusCode st) (LBS.fromChunks [W.statusMessage st])
    where
      go = do
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

data BasicAuth = BasicAuth { ba_user :: String
                           , ba_pass :: String
                           } deriving (Eq, Show)

instance Aeson.FromJSON BasicAuth where
    parseJSON (Aeson.Object v) = BasicAuth <$> v .: "user" <*> v .: "pass"
    parseJSON _ = fail "Wrong thing"

instance Aeson.ToJSON BasicAuth where
    toJSON v = Aeson.object [ "user" .= ba_user v, "pass" .= ba_pass v]

instance Monoid BasicAuth where
    mappend = const
    mempty = undefined

main = do
  args <- getArgs
  r_conn <- R.connect $ R.Client "127.0.0.1" "8081" LBS.empty

  auth <- R.get r_conn "admin" "basicauth" R.Default
  case auth of
    Just (a, _) -> do
              request <- basicauth (ba_user a) (ba_pass a) <$> HE.parseUrl "http://stream.twitter.com/1/statuses/sample.json" 
              withSocketsDo . HE.withHttpEnumerator . DE.run_ $ HE.httpRedirect request (httpiter r_conn)

    Nothing -> putStrLn "Can't find auth details in admin/basicauth"

{-
-- Convert a Network.OAuth.Http.Request into a Network.HTTP.Enumerator.Request
-- What. A. Pain.
http_cvt_request :: O.Request -> HE.Request
http_cvt_request oar = HE.Request method secure host port path query headers body
    where method = C8.pack . show . O.method $ oar
          secure = O.ssl oar
          host = C8.pack . O.host $ oar
          port = O.port oar
          path = C8.pack . intercalate "/" $ O.pathComps oar
          query = packpair <$> (O.toList . O.qString $ oar)
          headers = (first W.mkCIByteString) . packpair <$> (O.toList . O.reqHeaders $ oar)
          body = O.reqPayload oar

-- Convert a Network.HTTP.Enumerator.Response into a Network.OAuth.Http.Response
-- See above.
http_cvt_response :: HE.Response -> O.Response
http_cvt_response her = O.RspHttp status reason headers payload
    where status = HE.statusCode her
          reason = ""
          headers = O.fromList $ (unpackpair . first W.ciOriginal) <$> HE.responseHeaders her
          payload = HE.responseBody her

mappair f (a,b) = (f a, f b)
packpair = mappair C8.pack
unpackpair = mappair C8.unpack

newtype HttpOAuthStream a m b = HttpOAuthStream { iter :: W.Status -> W.ResponseHeaders -> DE.Iteratee a m b }

instance MonadIO m => O.HttpClient (HttpOAuthStream a m b) where
    --   runClient :: (MonadIO m) => c -> Request -> m (Either String Response)
    runClient c r = liftM cvt $ DE.run $ HE.http (http_cvt_request r) (iter c)
        where
          cvt :: Show a => Either a HE.Response -> Either String O.Response
          cvt (Left a) = Left $ show a
          cvt (Right r) = Right $ http_cvt_response r

data HttpOAuth = HttpOAuth { }

instance O.HttpClient HttpOAuth where
    runClient c r = (HE.httpLbs . http_cvt_request) r >>= return . cvt
        where 
          cvt :: HE.Response -> Either String O.Response
          cvt r@(HE.Response st _ b) | 200 <= st && st < 300 = Right $ http_cvt_response r
                                     | otherwise             = Left $ "HTTP status" ++ show st
-}
