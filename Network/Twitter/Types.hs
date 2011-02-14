{-# LANGUAGE OverloadedStrings #-}

module Network.Twitter.Types
    ( TweetURL(..)
    , TweetHashtag(..)
    , TwitterID(..)
    , TwitterUser(..)
    , TweetEntities(..)
    , TwitterTime(..)
    , Tweet(..)
    ) where

import Network.URI (URI(..), parseURI)

import Control.Applicative ((<$>), (<*>), (<|>), pure)

import Data.Monoid

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson.Types ((.=), (.:), (.:?), FromJSON(..), ToJSON(..))
import qualified Data.Aeson.Parser as Aeson

import qualified Data.Text as T
import Data.Maybe (fromJust, catMaybes)

import qualified Data.Time.Format as DT
import qualified Data.Time.Clock as DT
import System.Locale (defaultTimeLocale)

(.=?) :: ToJSON a => T.Text -> Maybe a -> Maybe Aeson.Pair
_ .=? Nothing = Nothing
name .=? (Just x) = Just $ name .= x

object' a b = Aeson.object $ a ++ catMaybes b

instance FromJSON URI where
    parseJSON (Aeson.String v) = case (parseURI . T.unpack) v of
                             Just uri -> pure uri
                             Nothing -> fail "Invalid URI"
    parseJSON _ = fail "Wrong thing"

instance ToJSON URI where
    toJSON = toJSON . show

data TweetURL = TweetURL { url :: URI
                         , displayUrl :: Maybe T.Text
                         , longUrl :: Maybe URI
                         }
                deriving (Eq, Show)

instance FromJSON TweetURL where
    parseJSON (Aeson.Object v) = TweetURL <$> v .: "url"
                                          <*> v .:? "display_url"
                                          <*> v .:? "expanded_url"
    parseJSON _ = fail "Wrong thing"

instance ToJSON TweetURL where
    toJSON u = object' [ "url" .= url u ] [ "display_url" .=? displayUrl u
                                          , "expanded_url" .=? longUrl u ]

newtype TweetHashtag = TweetHashtag { text :: T.Text }
    deriving (Eq, Show)

instance FromJSON TweetHashtag where
    parseJSON (Aeson.Object v) = TweetHashtag <$> v .: "text"
    parseJSON _ = fail "Wrong thing"

newtype TwitterID = TwitterID { twitterid :: Integer }
    deriving (Show, Eq)

-- Parse a string or number into an ID, but Strings are preferable
-- because Aeson looses precision on large Integers by using Double
instance FromJSON TwitterID where
    parseJSON (Aeson.String v) = pure $ TwitterID $ (read . T.unpack) v
    parseJSON (Aeson.Number v) = pure $ TwitterID $ floor v
    parseJSON _ = fail "Wrong thing"

-- Aeson can't currently deal with large Integer Twitter IDs, so
-- generate a string
instance ToJSON TwitterID where
    toJSON = toJSON . show . twitterid

instance ToJSON TweetHashtag where
    toJSON t = Aeson.object [ "text" .= text t ]

data TwitterUser = TwitterUser { tu_screen_name :: T.Text
                               , tu_name :: T.Text
                               , tu_id :: TwitterID
                               , tu_url :: Maybe URI
                               , tu_location :: Maybe T.Text
                               , tu_verified :: Bool
                               , tu_protected :: Bool
                               } deriving (Eq, Show)

instance FromJSON TwitterUser where
    parseJSON (Aeson.Object v) = TwitterUser <$> v .: "screen_name"
                                             <*> v .: "name"
                                             <*> v .: "id_str"
                                             <*> v .:? "url"
                                             <*> v .:? "location"
                                             <*> (v .: "verified" <|> pure False)
                                             <*> (v .: "protected" <|> pure False)
    parseJSON _ = fail "Wrong thing"

instance ToJSON TwitterUser where
    toJSON t = object' [ "screen_name" .= tu_screen_name t
                       , "name" .= tu_name t
                       , "id_str" .= tu_id t
                       , "location" .= tu_location t
                       , "verified" .= tu_verified t
                       , "protected" .= tu_protected t ]
                            [ "url" .=? tu_url t ]

data TweetRange = TweetRange Int Int
                  deriving (Eq, Show)

instance ToJSON TweetRange where
    toJSON (TweetRange a b) = toJSON [a, b]

data TweetEntities = TweetEntities { te_urls :: [ TweetURL ]
                                   , te_hashtags :: [ TweetHashtag ] 
                                   , te_mentions :: [ TwitterUser ]
                                   } deriving (Eq, Show)

instance FromJSON TweetEntities where
    parseJSON (Aeson.Object v) = TweetEntities <$> v .: "urls"
                                               <*> v .: "hashtags"
                                               <*> v .: "user_mentions"
    parseJSON _ = fail "Wrong thing"

instance ToJSON TweetEntities where
    toJSON v = Aeson.object [ "urls" .= te_urls v
                            , "hashtags" .= te_hashtags v
                            , "user_mentions" .= te_mentions v
                            ]

newtype TwitterTime = TwitterTime { timedate :: DT.UTCTime }
    deriving (Eq, Show)

timeformat = "%a %b %d %X %Z %Y"

instance FromJSON TwitterTime where
    -- "Sun Feb 13 21:56:36 +0000 2011"
    parseJSON (Aeson.String t) = case DT.parseTime defaultTimeLocale timeformat (T.unpack t) of
                                   Just x -> pure $ TwitterTime x
                                   Nothing -> fail "Bad date/time"
    parseJSON _ = fail "Wrong thing"

instance ToJSON TwitterTime where
    toJSON v = toJSON $ DT.formatTime defaultTimeLocale timeformat (timedate v)

data Tweet = Tweet { t_id :: TwitterID
                   , t_source :: T.Text
                   , t_user :: TwitterUser
                   , t_truncated :: Bool
                   , t_entities :: TweetEntities
                   , t_text :: T.Text
                   , t_created_at :: TwitterTime
                   , t_reply_status :: Maybe TwitterID
                   , t_reply_user :: Maybe TwitterID
                   , t_reply_screenname :: Maybe T.Text
                   , t_retweet :: Maybe Tweet
                   } deriving (Eq, Show)

instance FromJSON Tweet where
    parseJSON (Aeson.Object v) = Tweet <$> v .: "id_str"
                                       <*> v .: "source"
                                       <*> v .: "user"
                                       <*> v .: "truncated"
                                       <*> (v .: "entities" <|> pure (TweetEntities [] [] []))
                                       <*> v .: "text"
                                       <*> v .: "created_at"
                                       <*> v .:? "in_reply_to_status_id"
                                       <*> v .:? "in_reply_to_user_id"
                                       <*> v .:? "in_reply_to_screen_name"
                                       <*> v .:? "retweeted_status"
    parseJSON _ = fail "Wrong thing"

instance ToJSON Tweet where
    toJSON v = object' [ "id_str" .= t_id v
                       , "source" .= t_source v
                       , "user" .= t_user v
                       , "text" .= t_text v
                       , "created_at" .= t_created_at v
                       , "entities" .= t_entities v
                       , "truncated" .= t_truncated v ]
                       [ "in_reply_to_status_id" .=? t_reply_status v
                       , "in_reply_to_user_id" .=? t_reply_user v
                       , "in_reply_to_screen_name" .=? t_reply_screenname v
                       , "retweeted_status" .=? t_retweet v
                       ]

instance Monoid Tweet where
    mappend a _ = a
    mempty = undefined

data TweetDelete  = TweetDelete { td_statusid :: TwitterID
                                , td_userid :: TwitterID
                                } deriving (Eq, Show)


