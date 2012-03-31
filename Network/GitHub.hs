{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.GitHub where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Attoparsec.Lazy (parse, Result(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as B64
import Data.CaseInsensitive
import Data.Text (Text)
import Network.HTTP.Enumerator

-- | Construct a request from a `username:password` bytestring (suitable for a
-- Basic Auth scheme), a URI (starting with a `/`, e.g. `/user/repos`), and a
-- list of parameters.
apiGetRequest :: B.ByteString -> String
  -> [(CI B.ByteString, B.ByteString)] -> IO (Request IO)
apiGetRequest usernamePassword uri parameters = do
  let auth = "Basic " `B.append` B64.encode usernamePassword
  request <- parseUrl $ "https://api.github.com" ++ uri
  let request' = request
        { requestHeaders = ("Authorization", auth) : parameters }
  return request'

-- | Execute a GET agains the specified URI (e.g. `/user/repos`) using the
-- supplied `username:password` and parameters.
apiGet :: FromJSON a => String -> String
  -> [(CI B.ByteString, B.ByteString)] -> IO (Maybe a)
apiGet usernamePassword uri parameters = do
  request <- apiGetRequest (B.pack usernamePassword) uri parameters
  Response{..} <- withManager $ httpLbs request
  case parse json responseBody of
    Done _ value -> do
--      print value
      case fromJSON value of
        Success value' -> do
          return $ Just value'
        _ -> return Nothing
    _ -> return Nothing

-- | Return the list of repositories for a given `username:password` string.
repositoryList :: String -> IO (Maybe [Repository])
repositoryList usernamePassword = apiGet usernamePassword "/user/repos" []

-- | Represent a repository. TODO add missing fields.
data Repository = Repository
  { repositoryName :: Text
  , repositoryDescription :: Text
  }
  deriving Show

instance FromJSON Repository where
  parseJSON (Object v) = Repository <$>
    v .: "name" <*>
    v .: "description"
  parseJSON _ = mzero

