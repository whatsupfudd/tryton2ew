module Generation.Utils where

import qualified Data.Text as T

import qualified Parsing.Python as Py


fieldNamed :: T.Text -> Py.Field -> Bool
fieldNamed name field =
  field.name == name
