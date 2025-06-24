{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Generation.HsLib where

import qualified Data.ByteString as Bs
import qualified Data.Map as Mp
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath ((</>))

import Text.RawString.QQ (r)

import qualified Parsing.Xml as Xm
import qualified Parsing.Python as Py
import qualified Generation.Sql as Sq
import qualified Generation.EwTypes as Ew

import Generation.Utils (upperSnake)

genSqlFile :: FilePath -> Bs.ByteString -> [Ew.SqlFct] -> IO ()
genSqlFile dirPath moduleName sqlFcts =
  let
    filePath = dirPath </> (T.unpack . T.decodeUtf8 . upperSnake) moduleName <> ".hs"
    modDef = "module Lib." <> upperSnake moduleName <> " where\n"
    tmpl_imports =[r|
import qualified Data.ByteString as Bs
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime, TimeOfDay, DiffTime, LocalTime)
import Data.Vector (Vector)

import Hasql.Session (Session, statement)
import Hasql.Pool (Pool, use)
import qualified Hasql.TH as TH
    |]
    body = Bs.intercalate "\n\n" [ aFct.codeSF | aFct <- sqlFcts]
  in
  Bs.writeFile filePath (modDef <> tmpl_imports <> "\n\n" <> body)


genSqlOps :: Mp.Map T.Text Sq.SqlTable -> Mp.Map T.Text Py.TrytonModel -> Mp.Map T.Text (Ew.SqlFct, Ew.SqlFct)
genSqlOps tableMap ttModels =
  Mp.map (\tableDef ->
      let
        fetcher = sqlGenFetcher tableDef ttModels
        inserter = sqlGenInserter tableDef ttModels
      in
      (fetcher, inserter)
    ) tableMap


-- TODO: figure out how to make explicit the order of the fields in select.
sqlGenFetcher :: Sq.SqlTable -> Mp.Map T.Text Py.TrytonModel -> Ew.SqlFct
sqlGenFetcher tableDef ttModels =
  let
    sqlTableName = Sq.modelToSqlName tableDef.nameST
    fctName = sqlTableName <> "_fetch"
    joints = filter (\field -> case field.kindSF of Sq.RelationSFK _ -> True; _ -> False) tableDef.fieldsST
    nonJoints = filter (\field -> case field.kindSF of Sq.RelationSFK _ -> False; _ -> True) tableDef.fieldsST
    fields = map (\field ->
        let
          fieldType = sqlToHasqlType field.kindSF
        in
        Sq.quoteReservedWord field.oriName <> "::" <> fieldType <> if field.required then "" else "?"
      ) nonJoints
    outType = genTypeList nonJoints
    hsOutType = if length tableDef.fieldsST < 2 then outType else "(" <> outType <> ")"
  in
  -- TODO: add joint resolution.
  Ew.SqlFct {
    nameSF = fctName
    , codeSF =
        fctName <> " :: Int32 -> Int32 -> Session (Vector " <> hsOutType <> ")\n"
        <> fctName <> " offset limit =\n"
        <> "  statement (offset, limit) [TH.vectorStatement|\n    select "
        <> Bs.intercalate ", " fields
        <> "\n     from " <> sqlTableName
        <> "\n     offset $1::int4 limit $2::int4"
        <> "\n  |]\n\n"
  }


sqlGenInserter :: Sq.SqlTable -> Mp.Map T.Text Py.TrytonModel -> Ew.SqlFct
sqlGenInserter tableDef ttModels =
  let
    sqlTableName = Sq.modelToSqlName tableDef.nameST
    fctName = sqlTableName <> "_insert"
    joints = filter (\field -> case field.kindSF of Sq.RelationSFK _ -> True; _ -> False) tableDef.fieldsST
    nonJoints = filter (\field -> case field.kindSF of Sq.RelationSFK _ -> False; _ -> True) tableDef.fieldsST
    values = zipWith (\field position ->
       let
        fieldType = sqlToHasqlType field.kindSF
       in
       "$" <> (T.encodeUtf8 . T.pack . show) position <> "::"
       <> fieldType <> if field.required then "" else "?"
       )
        nonJoints
        [1..]
    inType = genTypeList nonJoints
    hsInType = if length tableDef.fieldsST < 2 then inType else "(" <> inType <> ")"
  in
  Ew.SqlFct {
    nameSF = fctName
    , codeSF = fctName <> " :: " <> hsInType <> " -> Session (Int32)\n"
        <> fctName <> " values =\n"
        <> "  statement values [TH.singletonStatement|\n    insert into "
        <> sqlTableName
        <> "\n      (" <> Bs.intercalate ", " [ Sq.quoteReservedWord f.oriName | f <- nonJoints ]
        <> ")\n      values (" <> Bs.intercalate ", " values <> ")\n    returning id\n  |]\n\n"
  }


genTypeList :: [Sq.SqlFieldDef] -> Bs.ByteString
genTypeList fields =
  Bs.intercalate ", " $ map (\f ->
    let
      baseType = sqlToHsType f.kindSF
    in
    if f.required then baseType else "Maybe " <> baseType
  ) fields


sqlToHasqlType :: Sq.SqlFieldKind -> Bs.ByteString
sqlToHasqlType = \case
  Sq.VarCharSFK -> "text"
  Sq.CharSFK -> "text"
  Sq.TextSFK -> "text"
  Sq.FullTextSFK -> "text"
  Sq.BooleanSFK -> "bool"
  Sq.NumericSFK -> "int4"
  Sq.IntegerSFK -> "int4"
  Sq.FloatSFK -> "float"
  Sq.DateSFK -> "date"
  Sq.DateTimeSFK -> "timestamp"
  Sq.TimeSFK -> "time"
  Sq.TimeDeltaSFK -> "interval"
  Sq.TimestampSFK -> "timestamp"
  Sq.EnumSFK -> "enum"
  Sq.MultiValueSFK -> "text"
  Sq.SelectionSFK -> "text"
  Sq.MultiSelectionSFK -> "text"
  Sq.DictSFK -> "json"
  Sq.BinarySFK -> "bytea"
  Sq.RelationSFK _-> "int4"
  Sq.FunctionSFK -> "text"
  Sq.CommentSFK _ -> "text"
  -- If the following is not flagged as "redundant choice", it means some values are missing from the case statement.
  _ -> "Unknown"


sqlToHsType :: Sq.SqlFieldKind -> Bs.ByteString
sqlToHsType = \case
  Sq.VarCharSFK -> "Text"
  Sq.CharSFK -> "Text"
  Sq.TextSFK -> "Text"
  Sq.FullTextSFK -> "Text"
  Sq.BooleanSFK -> "Bool"
  Sq.NumericSFK -> "Int32"
  Sq.IntegerSFK -> "Int32"
  Sq.FloatSFK -> "Float"
  Sq.DateSFK -> "Day"
  Sq.DateTimeSFK -> "UTCTime"
  Sq.TimeSFK -> "TimeOfDay"
  Sq.TimeDeltaSFK -> "DiffTime"
  Sq.TimestampSFK -> "UTCTime"
  Sq.EnumSFK -> "Text"
  Sq.MultiValueSFK -> "Text"
  Sq.SelectionSFK -> "Text"
  Sq.MultiSelectionSFK -> "Text"
  Sq.DictSFK -> "Value"
  Sq.BinarySFK -> "ByteString"
  Sq.RelationSFK _ -> "Int32"
  Sq.FunctionSFK -> "Text"
  Sq.CommentSFK _ -> "Text"
  -- If the following is not flagged as "redundant choice", it means some values are missing from the case statement.
  _ -> "Unknown"
