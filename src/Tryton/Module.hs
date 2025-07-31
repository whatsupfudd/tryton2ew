module Tryton.Module where

import Data.Text (Text)
import qualified Data.ConfigFile as Cf


data ModuleSrcTT = ModuleSrcTT {
  nameMT :: String
  , locationMT :: Text
  , dependsMT :: [String]
  , targetMT :: Maybe FilePath
  , dataSpecMT :: [FilePath]
  , supportSpecMT :: [FilePath]
  , localesMT :: [FilePath]
  , viewsMT :: [FilePath]
  , miscXmlMT :: [FilePath]
  , modelsMT :: [FilePath]
  , logicMT :: [FilePath]
  }
  deriving (Show)