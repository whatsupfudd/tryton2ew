module Generation.Utils where

import qualified Data.ByteString as Bs

import qualified Parsing.Python as Py


fieldNamed :: Bs.ByteString -> Py.Field -> Bool
fieldNamed name field =
  field.name == name
