module Generation.Utils where

import qualified Data.ByteString as Bs
import qualified Data.Text as T
import qualified Data.Char as C

import qualified Parsing.Python as Py


fieldNamed :: Bs.ByteString -> Py.Field -> Bool
fieldNamed name field =
  field.name == name


capitalize :: T.Text -> T.Text
capitalize aWord = T.cons (C.toUpper $ T.head aWord) (T.tail aWord)

