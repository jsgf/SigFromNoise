{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns #-}

module Main (main) where

import System.Environment (getArgs)

import Control.Monad
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Loops (whileM_)
import Control.Applicative (Applicative(..), (<$>), (<*>))
import Data.Monoid (Monoid(..))

import qualified Data.Attoparsec.Enumerator as AE

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.:), (.=), (./))

import qualified Data.Enumerator as DE

import Network (withSocketsDo)
import qualified Network.HTTP.Enumerator as HE
import qualified Network.Wai as W
import Network.URI (escapeURIString)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base64 as B64

import qualified Network.Riak.Basic as R (connect, delete, foldKeys)
import qualified Network.Riak.Types as R
import qualified Network.Riak.Content as R (Content(..), Link, link)
import qualified Network.Riak.Value.Monoid as R

import qualified Data.Text as T
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Function (on)

import Network.Twitter.Types

import qualified Network.OAuth.Consumer as O
import qualified Network.OAuth.Http.HttpClient as O
import qualified Network.OAuth.Http.Request as O
import qualified Network.OAuth.Http.Response as O

basicauth :: String -> String -> HE.Request -> HE.Request
basicauth u p r = r { HE.requestHeaders = basicauthhdr u p : HE.requestHeaders r }

basicauthhdr u p = (W.mkCIByteString "authorization", C8.concat [ "Basic ", auth ])
    where auth = B64.encode $ C8.concat $ map C8.pack [ u, ":",  p ]

idToKey :: TwitterID -> R.Key
idToKey = LC8.pack . show . twitterid

tweetKey :: Tweet -> R.Key
tweetKey = idToKey . t_id

userProfileKey :: TwitterUserProfile -> R.Key
userProfileKey = idToKey . tup_id

userKey :: TwitterUser -> R.Key
userKey = idToKey . tu_id

data ContentT a = ContentT { content :: a
                           , links :: Set.Set R.Link
                           --, usermeta :: S.Seq R.Pair
                           } deriving (Eq, Show)

mkContentT :: a -> ContentT a
mkContentT a = ContentT a Set.empty

instance Monoid a => Monoid (ContentT a) where
    mempty = ContentT mempty Set.empty
    a `mappend` b = ContentT ((mappend `on` content) a b) ((mappend `on` links) a b)

instance Functor ContentT where
    fmap f a = a { content = f (content a) }

instance Applicative ContentT where
    pure = mkContentT
    (ContentT f l1) <*> (ContentT x l2) = ContentT (f x) (l1 `mappend` l2)

instance (Aeson.FromJSON a, Aeson.ToJSON a) => R.IsContent (ContentT a) where
    parseContent c = mkContentT `fmap` (R.parseContent c >>= Aeson.parseJSON)
    toContent o = c { R.links = (S.fromList . Set.toList . links) o }
        where c = (R.toContent . Aeson.toJSON . content) o

addlinks :: [R.Link] -> ContentT a -> ContentT a
addlinks l ct = ct { links = links ct `mappend` Set.fromList l }

{-
  Store tweet to database, along with extra info:
  - Tweets/: TweetID -> Tweet           -- tweets themselves
  - TweetUsers/: UserID -> [TweetID]    -- users who actually tweet (?RT?)
  - TweetMentions/: UserID -> [TweetID] -- users mentioned in tweets
  - TweetURLs/: URL -> [TweetID]        -- urls referenced by tweets

  Other buckets:
  - TwitterUserProfiles/: UserID -> TwitterUserProfile  -- complete profile info of user
  - Users/: UserID -> User                              -- local user info (User ID same as Twitter's)

  This should probably use Riak links for better querying...
-}

urlkeys :: Tweet -> [R.Key]
urlkeys t = (urlkey . besturl) `fmap` (Set.toList . te_urls . t_entities) t
    where urlkey = LC8.pack . (escapeURIString (not . flip elem "/?&%")) . show
          besturl u = fromMaybe (url u) (expandedUrl u)

mentionkeys :: Tweet -> [R.Key]
mentionkeys t = userKey `fmap` (Set.toList . te_mentions . t_entities) t

getMulti :: (Monoid c, R.IsContent c) => R.Connection -> R.Bucket -> [R.Key] -> IO [Maybe (c, R.VClock)]
getMulti c b k = R.getMany c b k R.Default

updateMulti :: forall c. (Show c, Eq c, Monoid c, R.IsContent c) =>
               R.Connection -> R.Bucket -> [R.Key] -> [c] -> IO [(c, R.VClock)]
updateMulti c b k v = do vc <- map (snd `fmap`) `liftM` (getMulti c b k :: IO [Maybe (c, R.VClock)])
                         let kvcv = zip3 k vc v
                         putStrLn $ "Updating " ++ show b ++ " with " ++ show kvcv
                         R.putMany c b kvcv R.Default R.Default

getUrls :: R.Connection -> [R.Key] -> IO [Maybe (ContentT TwitterID, R.VClock)]
getUrls c k = getMulti c "TweetURLs" k

type TweetIDs = Set.Set TwitterID

updateUrls :: R.Connection -> [R.Key] -> [ContentT TweetIDs] -> IO [(ContentT TweetIDs, R.VClock)]
updateUrls c k v = updateMulti c "TweetURLs" k v

getMentions :: R.Connection -> [R.Key] -> IO [Maybe (ContentT TweetIDs, R.VClock)]
getMentions c k = getMulti c "TweetMentions" k

updateMentions :: R.Connection -> [R.Key] -> [ContentT TweetIDs] -> IO [(ContentT TweetIDs, R.VClock)]
updateMentions c k v = updateMulti c "TweetMentions" k v

updateTweet :: R.Connection -> R.Key -> ContentT Tweet -> IO (ContentT Tweet, R.VClock)
updateTweet c k v = head `liftM` updateMulti c "Tweets" [k] [v]

updateTweetUser :: R.Connection -> R.Key -> ContentT TweetIDs -> IO (ContentT TweetIDs, R.VClock)
updateTweetUser c k v = head `liftM` updateMulti c "TweetUsers" [k] [v]

addTweetLinks :: ContentT Tweet -> ContentT Tweet
addTweetLinks ct@(content -> t) = addlinks (catMaybes . map (mklink t) $ l) ct
    where 
      mklink t (f, b, r) = R.link <$> pure b <*> f t <*> pure r
      l = [ (fmap idToKey . t_reply_status, "Tweets", "reply")
          , (fmap tweetKey . t_retweet, "Tweets", "retweet") ]

mkTweet :: Tweet -> ContentT Tweet
mkTweet = addTweetLinks . mkContentT

stashTweet :: R.Connection -> Tweet -> IO ()
stashTweet c t = do updateTweet c (tweetKey t) jt
                    updateTweetUser c (userKey . t_user $ t) tweetid
                    updateUrls c urls (repeat tweetid)
                    updateMentions c mentions (repeat tweetid)
                    return ()
                    where jt = addlinks (mentionlinks ++ urllinks) . mkTweet $ t
                          tweetlink = R.link "Tweets" (tweetKey t) "tweet"
                          tweetid :: ContentT (Set.Set TwitterID)
                          tweetid = addlinks [tweetlink] $ (mkContentT . Set.singleton . t_id $ t)

                          mentions = mentionkeys t
                          mentionlinks = map (\k -> R.link "TweetUsers" k "mention") mentions
                          urls = urlkeys t
                          urllinks = map (\k -> R.link "TweetURLs" k "url") urls

untilDone :: forall a a1. DE.Iteratee a1 IO a -> DE.Iteratee a1 IO ()
untilDone = whileM_ $ liftM not DE.isEOF

httpiter :: R.Connection -> W.Status -> t -> DE.Iteratee C8.ByteString IO ()
httpiter conn st _ | st == W.status200 = untilDone go
                   | otherwise =
                       DE.throwError $ HE.StatusCodeException (W.statusCode st)
                                                (LBS.fromChunks [W.statusMessage st])
    where
      go = do
            x <- AE.iterParser Aeson.json

            case x ./ "delete" ./ "status" ./ "id_str" of
              (Aeson.String str) -> liftIO $ do
                                      putStrLn $ "Delete status " ++ T.unpack str
                                      deletekeys conn "Tweets" [key]
                                             where key =  (LC8.pack . T.unpack) str
              _ -> case Aeson.fromJSON x :: Aeson.Result Tweet of
                     Aeson.Success a -> liftIO $ do
                                          --putStrLn $ show a
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

riak_conn :: IO R.Connection
riak_conn = R.connect $ R.Client "127.0.0.1" "8081" LBS.empty

main :: IO ()
main = do
  r_conn <- riak_conn

  auth <- R.get r_conn "admin" "basicauth" R.Default
  case auth of
    Just (ja, _) -> do
              request <- basicauth (ba_user a) (ba_pass a) <$> HE.parseUrl "http://stream.twitter.com/1/statuses/sample.json" 
              withSocketsDo . HE.withHttpEnumerator . DE.run_ $ HE.httpRedirect request (httpiter r_conn)
                  where a = content ja

    Nothing -> putStrLn "Can't find auth details in admin/basicauth"

allkeys :: R.Connection -> R.Bucket -> IO [R.Key]
allkeys conn bucket = R.foldKeys conn bucket (\a k -> return (k:a)) []

deletekeys :: R.Connection -> R.Bucket -> [R.Key] -> IO ()
deletekeys conn bucket = mapM_ $ \k -> R.delete conn bucket k R.Default

deleteall :: R.Connection -> R.Bucket -> IO ()
deleteall c b = do keys <- allkeys c b
                   deletekeys c b keys

countkeys :: R.Connection -> R.Bucket -> IO Int
countkeys c b = do keys <- allkeys c b
                   return $ length keys

wipeeverything :: IO ()
wipeeverything = do c <- riak_conn
                    putStrLn "Wiping Tweets"
                    deleteall c "Tweets"
                    putStrLn "Wiping TweetUsers"
                    deleteall c "TweetUsers"
                    putStrLn "Wiping TweetMentions"
                    deleteall c "TweetMentions"
                    putStrLn "Wiping TweetURLs"
                    deleteall c "TweetURLs"

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
