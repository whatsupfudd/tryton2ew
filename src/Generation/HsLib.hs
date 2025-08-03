{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Generation.HsLib where

import qualified Data.ByteString as Bs
import Data.Either (isLeft)
import qualified Data.Map as Mp
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath ((</>))

import Text.RawString.QQ (r)

import qualified Parsing.Xml as Xm
import qualified Parsing.Python as Py
import qualified Generation.Sql as Sq
import qualified Generation.EwTypes as Ew
import qualified Generation.Utils as U

import Generation.Utils (upperSnake)

genSqlFctFile :: FilePath -> Bs.ByteString -> Bs.ByteString -> [Either String Ew.SqlFct] -> IO ()
genSqlFctFile dirPath modContainer modName sqlFcts =
  let
    filePath = dirPath </> (T.unpack . T.decodeUtf8 . upperSnake) modName <> ".hs"
    modDef = "{-# LANGUAGE QuasiQuotes #-}\n{-# LANGUAGE DeriveGeneric #-}\n{-# LANGUAGE DeriveAnyClass #-}\n\nmodule " <> modContainer <> upperSnake modName <> " where\n"
    tmpl_imports =[r|
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime, TimeOfDay, DiffTime, LocalTime, Day)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Functor.Contravariant ((>$<))
import Data.Profunctor (rmap)

import GHC.Generics (Generic)

import Data.Aeson (encode, FromJSON (..), ToJSON, fromJSON, Result(..), withObject, (.:))

import Hasql.Session (Session, statement)
import Hasql.Statement (Statement (..))
import Hasql.Pool (Pool, use)
import qualified Hasql.TH as TH
import qualified Hasql.Session as H
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE

import Wapp.AppDef (NativeLibFunction)
import Wapp.Apps.GnuHealth.Types

    |]
    body = Bs.intercalate "\n\n" [ case aFct of
        Left err -> "-- @[genSqlFctFile] " <> (T.encodeUtf8 . T.pack) err <> "\n"
        Right fct -> fct.codeSF
      | aFct <- sqlFcts
      ]
  in
  Bs.writeFile filePath (modDef <> tmpl_imports <> "\n\n" <> body)


genSqlOps :: Mp.Map T.Text Sq.SqlTable -> Mp.Map Bs.ByteString Py.TrytonModel -> Mp.Map Bs.ByteString (Either String Ew.SqlFct, Either String Ew.SqlFct)
genSqlOps tableMap ttModels =
  Mp.mapKeys T.encodeUtf8 $ Mp.map (\tableDef ->
      let
        fetcher = sqlGenFetcher tableDef ttModels
        inserter = sqlGenInserter tableDef ttModels
      in
      (fetcher, inserter)
    ) tableMap


genFctDispatcher :: FilePath -> Mp.Map Bs.ByteString (Either String Ew.SqlFct, Either String Ew.SqlFct) -> IO ()
genFctDispatcher filePath sqlOps =
  let
    header = [r|module Wapp.Apps.GnuHealth.FctDispatcher where

import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Mp
import Data.Vector (Vector)

import qualified Data.Aeson as Ae

import Hasql.Pool (Pool, use)
import Hasql.Session (Session, statement)

import qualified Wapp.AppDef as Wd

import Wapp.Apps.GnuHealth.Types
import qualified Wapp.Apps.GnuHealth.DB as Gdb

dispatch :: Wd.NativeLibFunction
dispatch dbPool (aeValue, mbLabel) =
  case Ae.fromJSON aeValue :: Ae.Result TreeFetchRequest of
    Ae.Error err -> do
      pure . Left . show $ "@[dispatch] invalid request: " <> err
    Ae.Success fetchRequest ->
      case findTargetFct fetchRequest.treeNameTP of
        Nothing -> pure . Left . show $ "@[dispatch] unknown function: " <> fetchRequest.treeNameTP
        Just targetFct -> do
          rezA <- targetFct dbPool fetchRequest
          case rezA of
            Left err -> pure . Left . show $ err
            Right rows ->
              pure $ Right rows

findTargetFct :: Text -> Maybe (Pool -> TreeFetchRequest -> IO (Either String Lbs.ByteString))
findTargetFct fctName =
  case fctName of
|]
    footer = [r|
    _ -> Nothing
|]


    caseItemSpacing = Bs.replicate 4 32
    elmEventName fctName = U.convertModuleNameToHsFctName fctName <> "_tree"
    nativeFctName fctName = "Gdb." <> U.convertModuleNameToHsFctName fctName <> "_fetch"
    body = Bs.intercalate "\n" $ mapMaybe (\(fctName, (fetcher, inserter)) ->
      if isLeft fetcher then
        Nothing
      else
        Just $ caseItemSpacing <> "\"" <> elmEventName fctName <> "\" -> Just " <> nativeFctName fctName
      ) (Mp.toList sqlOps)
  in
  Bs.writeFile filePath (header <> body <> footer)


genDbModuleImport :: FilePath -> [Bs.ByteString] -> IO ()
genDbModuleImport filePath sqlFctNames =
  let
    header = "module Wapp.Apps.GnuHealth.DB (\n  "
    hsModuleNames = map (\fn -> "Wapp.Apps.GnuHealth.DB." <> U.convertModuleNameGhToHs fn) sqlFctNames
    exports = Bs.intercalate "\n  , " $ map ("module " <>) hsModuleNames
    imports = Bs.intercalate "\n" $ map ("import " <>) hsModuleNames
  in
  Bs.writeFile filePath (header <> exports <> ") where\n\n" <> imports)


genNativeLibDef :: FilePath -> IO ()
genNativeLibDef filePath =
  let
    header = "-- import Wapp.Apps.GnuHealth.FctDispatcher as Ghd\n\n"
    body = "  , (\"gnuhealth.db\", Mp.fromList [(\"general_tree_loader\", Ghd.dispatch)]\n)\n"
  in
  Bs.writeFile filePath (header <> body)


-- TODO: figure out how to make explicit the order of the fields in select.
sqlGenFetcher :: Sq.SqlTable -> Mp.Map Bs.ByteString Py.TrytonModel -> Either String Ew.SqlFct
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
    hsOutType = genTypeList nonJoints
    outDataName = "Out_" <> sqlTableName
    encoderType = genDataType outDataName nonJoints
    encoderName = "enc_" <> sqlTableName
    encoderFct = genEncoder encoderName outDataName nonJoints
    tplEncoderName = "tpl_" <> encoderName
    encoderTupleFct = genTupleEncoder tplEncoderName outDataName nonJoints
    -- hsOutType = if length nonJoints < 2 then outType else "(" <> outType <> ")"
    -- Int32 -> Int32 -> Session (Vector " <> hsOutType <> ")
    fctDef = fctName <> " :: Pool -> TreeFetchRequest -> IO (Either String Lbs.ByteString)\n"
              <> fctName <> " dbPool fetchReq = do\n"
              <> "  ieRezA <- use dbPool $ statement (fetchReq.offsetTP, fetchReq.limitTP) $\n"
    statement = "    rmap (fmap " <> tplEncoderName <> ") "
          <> "[TH.vectorStatement|\n      select "
          <> Bs.intercalate ", " fields
          <> "\n       from " <> sqlTableName
          <> "\n       offset $1::int4 limit $2::int4"
          <> "\n    |]\n\n"
    ending = "  case ieRezA of\n"
            <> "    Left err -> pure . Left . show $ err\n"
            <> "    Right recList -> pure . Right . encode $ recList"
  in
  if null fields then
      Left $ "@[sqlGenFetcher] no fields for table: " <> (T.unpack . T.decodeUtf8) sqlTableName
  else if length fields > 64 then
    Left $ "@[sqlGenFetcher] too many fields for table: " <> (T.unpack . T.decodeUtf8) sqlTableName
  else
    -- TODO: add joint resolution.
    Right Ew.SqlFct {
      nameSF = fctName
      , codeSF =
          encoderType <> "\n\n"
          <> encoderFct <> "\n\n"
          <> encoderTupleFct <> "\n\n"
          <> fctDef
          <> statement
          <> ending
    }

sqlGenInserter :: Sq.SqlTable -> Mp.Map Bs.ByteString Py.TrytonModel -> Either String Ew.SqlFct
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
    hsInType = genTypeList nonJoints
    -- hsInType = if length tableDef.fieldsST < 2 then inType else "(" <> inType <> ")"
  in
  if null nonJoints || null values then
    Left $ "@[sqlGenInserter] no nonJoints or values for table: " <> (T.unpack . T.decodeUtf8) sqlTableName
  else if length nonJoints > 64 then
    Left $ "@[sqlGenInserter] too many fields for table: " <> (T.unpack . T.decodeUtf8) sqlTableName
 else
    Right Ew.SqlFct {
      nameSF = fctName
    , codeSF = fctName <> " :: " <> hsInType <> " -> Session (Int32)\n"
        <> fctName <> " values =\n"
        <> "  statement values [TH.singletonStatement|\n    insert into "
        <> sqlTableName
        <> "\n      (" <> Bs.intercalate ", " [ Sq.quoteReservedWord f.oriName | f <- nonJoints ]
        <> ")\n      values (" <> Bs.intercalate ", " values <> ")\n    returning id::int4\n  |]\n\n"
    }


genTypeList :: [Sq.SqlFieldDef] -> Bs.ByteString
genTypeList fields =
  let
    types = Bs.intercalate ", " $ map (\f ->
        let
          baseType = sqlToHsType f.kindSF
        in
        if f.required then baseType else "Maybe " <> baseType
      ) fields
  in
    case fields of
      [] -> "()"
      [a] -> if Sq.required (head fields) then types else "(" <> types <> ")"
      _ -> "(" <> types <> ")"


genDataType :: Bs.ByteString -> [Sq.SqlFieldDef] -> Bs.ByteString
genDataType typeName fields =
  let
    types = Bs.intercalate "\n  , " $ zipWith (\aField pos
      -> (let baseType = sqlToHsType aField.kindSF
          in
            "f_"
              <>
                (T.encodeUtf8 . T.pack . show) pos
                  <> " :: " <> if aField.required then baseType else "Maybe " <> baseType)) fields [1..]
    typeSpec = if length fields == 1 then "newtype" else "data"
    typeImpl = typeName <> " = " <> typeName <> " {\n  " <> types <> "\n  }"
    derivingClause = if null fields then "" else "\n  deriving (Generic, Show, ToJSON)"
  in
    typeSpec <> " " <> typeImpl <> derivingClause


genEncoder :: Bs.ByteString -> Bs.ByteString -> [Sq.SqlFieldDef] -> Bs.ByteString
genEncoder encoderName encoderType nonJoints =
  let
    fields = zipWith (\aField pos ->
      "HD.column " <> (if aField.required then "(HD.nonNullable HD." else "(HD.nullable HD.") <> sqlToHasqlType aField.kindSF <> ")"
      ) nonJoints [1..]
  in
  encoderName <> " =\n  " <> encoderType <> "\n   <$> " <> Bs.intercalate "\n   <*> " fields <> "\n"

genTupleEncoder :: Bs.ByteString -> Bs.ByteString -> [Sq.SqlFieldDef] -> Bs.ByteString
genTupleEncoder encoderName encoderType nonJoints =
  let
    params = zipWith (\aField pos -> "p" <> (T.encodeUtf8 . T.pack . show) pos ) nonJoints [1..]
    fields = zipWith (\aField pos -> "f_" <> (T.encodeUtf8 . T.pack . show) pos) nonJoints [1..]
  in
  encoderName <> " (" <> Bs.intercalate ", " params <> ") =\n  "
   <> encoderType <> " {\n    "
   <> Bs.intercalate "\n    , " (zipWith (\f p -> f <> " = " <> p) fields params)
   <> "\n  }"


sqlToHasqlType :: Sq.SqlFieldKind -> Bs.ByteString
sqlToHasqlType = \case
  Sq.VarCharSFK -> "text"
  Sq.CharSFK -> "text"
  Sq.TextSFK -> "text"
  Sq.FullTextSFK -> "text"
  Sq.BooleanSFK -> "bool"
  Sq.NumericSFK -> "int4"
  Sq.IntegerSFK -> "int4"
  Sq.FloatSFK -> "float4"
  Sq.DateSFK -> "date"
  Sq.DateTimeSFK -> "timestamp"
  Sq.TimeSFK -> "time"
  Sq.TimeDeltaSFK -> "interval"
  Sq.TimestampSFK -> "timestamptz"
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
  Sq.DateTimeSFK -> "LocalTime"
  Sq.TimeSFK -> "TimeOfDay"
  Sq.TimeDeltaSFK -> "DiffTime"
  Sq.TimestampSFK -> "UTCTime"
  Sq.EnumSFK -> "Text"
  Sq.MultiValueSFK -> "Text"
  Sq.SelectionSFK -> "Text"
  Sq.MultiSelectionSFK -> "Text"
  Sq.DictSFK -> "Value"
  Sq.BinarySFK -> "Bs.ByteString"
  Sq.RelationSFK _ -> "Int32"
  Sq.FunctionSFK -> "Text"
  Sq.CommentSFK _ -> "Text"
  -- If the following is not flagged as "redundant choice", it means some values are missing from the case statement.
  _ -> "Unknown"
