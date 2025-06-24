module Generation.Utils where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as BsC
import qualified Data.Text as T
import qualified Data.Char as C

import qualified Parsing.Python as Py


fieldNamed :: Bs.ByteString -> Py.Field -> Bool
fieldNamed name field =
  field.name == name


capitalize :: T.Text -> T.Text
capitalize aWord = T.cons (C.toUpper $ T.head aWord) (T.tail aWord)

upperSnake :: Bs.ByteString -> Bs.ByteString
upperSnake aWord = Bs.intercalate "_" (map (\w -> BsC.cons (C.toUpper (BsC.head w)) (BsC.tail w)) (BsC.split '_' aWord))
