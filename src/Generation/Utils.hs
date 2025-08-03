module Generation.Utils where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as BsC
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.Word8 as W8

import qualified Parsing.Python as Py


fieldNamed :: Bs.ByteString -> Py.Field -> Bool
fieldNamed name field =
  field.name == name


capitalize :: T.Text -> T.Text
capitalize aWord = T.cons (C.toUpper $ T.head aWord) (T.tail aWord)

capitalizeBs :: Bs.ByteString -> Bs.ByteString
capitalizeBs aWord = Bs.cons (W8.toUpper (Bs.head aWord)) (Bs.tail aWord)

upperSnake :: Bs.ByteString -> Bs.ByteString
upperSnake aWord = Bs.intercalate "_" (map (\w -> BsC.cons (C.toUpper (BsC.head w)) (BsC.tail w)) (BsC.split '_' aWord))

toLowerBs :: Bs.ByteString -> Bs.ByteString
toLowerBs = Bs.map W8.toLower

convertModuleNameToHsFctName :: Bs.ByteString -> Bs.ByteString
convertModuleNameToHsFctName oriName =
  Bs.intercalate "_" (concatMap (Bs.split 45) (Bs.split 46 oriName))  -- _ : 95, - : 45, . : 46


convertModuleNameGhToHs :: Bs.ByteString -> Bs.ByteString
convertModuleNameGhToHs oriName =
  let
    nameParts = concatMap (Bs.split 95) (concatMap (Bs.split 45) (Bs.split 46 oriName))  -- _ : 95, - : 45, . : 46
  in
  Bs.intercalate "_" (map capitalizeBs nameParts)

