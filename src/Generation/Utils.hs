module Generation.Utils where

import qualified Parsing.Python as Py


fieldNamed :: String -> Py.Field -> Bool
fieldNamed name field =
  field.name == name
