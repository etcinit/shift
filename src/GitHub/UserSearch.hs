{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Provides missing user search functionality from the GitHub client.
module GitHub.UserSearch where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)

import qualified Data.Text.Encoding as TE

import GitHub.Data

data UserResult = UserResult
  { _urLogin :: Text
  , _urId :: Int
  , _urAvatarUrl :: Text
  , _urGravatarId :: Text
  , _urHtmlUrl :: Text
  , _urType :: Text
  , _urSiteAdmin :: Bool
  , _urScore :: Double
  } deriving (Show)

instance FromJSON UserResult where
  parseJSON (Object v)
    = UserResult
      <$> v .: "login"
      <*> v .: "id"
      <*> v .: "avatar_url"
      <*> v .: "gravatar_id"
      <*> v .: "html_url"
      <*> v .: "type"
      <*> v .: "site_admin"
      <*> v .: "score"
  parseJSON invalid = typeMismatch "UserResult" invalid

makeLenses ''UserResult

searchUsersR :: Text -> Request k (SearchResult UserResult)
searchUsersR searchString = Query
  ["search", "users"]
  [("q", Just $ TE.encodeUtf8 searchString)]

