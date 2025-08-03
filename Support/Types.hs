{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Wapp.Apps.GnuHealth.Types where

import Control.Applicative

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.ByteString.Base64 as B64
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.UUID (UUID)

import Data.Aeson (FromJSON (..), ToJSON (..), ToJSONKey, FromJSONKey)

import GHC.Generics
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae

import Wapp.Utils.Json (cutFieldP1, cutFieldP2)

instance ToJSON Bs.ByteString where
    toJSON = toJSON . decodeUtf8 . B64.encode

instance FromJSON Bs.ByteString where
    parseJSON o = either fail return . B64.decode . encodeUtf8 =<< parseJSON o

instance ToJSON Lbs.ByteString where
    toJSON = toJSON . decodeUtf8 . B64.encode . Lbs.toStrict

instance FromJSON Lbs.ByteString where
    parseJSON o = either fail (return . Lbs.fromStrict) . B64.decode . encodeUtf8 =<< parseJSON o

instance ToJSONKey Bs.ByteString
instance FromJSONKey Bs.ByteString


-- | Entry point into the Aox mailboxes.
data TreeFetchRequest = TreeFetchRequest {
    treeNameTP :: Text
    , offsetTP :: Int32
    , limitTP :: Int32
  }
  deriving (Show, Eq, Generic)


instance Ae.FromJSON TreeFetchRequest where
  parseJSON = Ae.genericParseJSON cutFieldP2

instance Ae.ToJSON TreeFetchRequest where
  toJSON = Ae.genericToJSON cutFieldP2
