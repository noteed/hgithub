{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module      : Network.GitHub
-- Copyright   : (c) 2012 Vo Minh Thu,
--
-- License     : BSD-style
-- Maintainer  : thu@hypered.be
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides bindings to the GitHub API v3.
module Network.GitHub where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Attoparsec.Lazy (parse, Result(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as L
import Data.CaseInsensitive
import Data.Text (Text)
import qualified Data.Text as T
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

-- | Construct a request from a `username:password` bytestring (suitable for a
-- Basic Auth scheme), a URI (starting with a `/`, e.g. `/user/repos`), and a
-- body.
apiPostRequest :: B.ByteString -> String -> L.ByteString -> IO (Request IO)
apiPostRequest usernamePassword uri body = do
  let auth = "Basic " `B.append` B64.encode usernamePassword
  request <- parseUrl $ "https://api.github.com" ++ uri
  let request' = request
        { method = "POST"
        , requestHeaders = [("Authorization", auth)]
        , requestBody = RequestBodyLBS body
        }
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

-- | Execute a POST agains the specified URI (e.g. `/user/repos`) using the
-- supplied `username:password` and body.
apiPost :: FromJSON a => String -> String -> L.ByteString -> IO (Maybe a)
apiPost usernamePassword uri body = do
  request <- apiPostRequest (B.pack usernamePassword) uri body
  Response{..} <- withManager $ httpLbs request
  case parse json responseBody of
    Done _ value -> do
      print value
      case fromJSON value of
        Success value' -> do
          return $ Just value'
        _ -> return Nothing
    _ -> return Nothing

-- | Return the list of repositories for a given `username:password` string.
repositoryList :: String -> IO (Maybe [Repository])
repositoryList usernamePassword = apiGet usernamePassword "/user/repos" []

-- | Create a new repository from a given name and description.
repositoryCreate :: String -> String -> Maybe String -> IO (Maybe Repository)
repositoryCreate usernamePassword name description =
  apiPost usernamePassword "/user/repos" $ encode CreateRepository
    { createRepositoryName = T.pack name
    , createRepositoryDescription = T.pack <$> description
    }

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

-- | Data needed to create a new repository.
data CreateRepository = CreateRepository
  { createRepositoryName :: Text
  , createRepositoryDescription :: Maybe Text
  }
  deriving Show

instance ToJSON CreateRepository where
   toJSON CreateRepository{..} = object $
     [ "name" .= createRepositoryName
     ] ++ maybe [] ((:[]) . ("description" .=)) createRepositoryDescription
