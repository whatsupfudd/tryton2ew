{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant if" #-}
module Generation.Sql where

import qualified Data.ByteString as Bs
import qualified Data.Char as C
import Data.Either (rights, lefts)
import qualified Data.List as L
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Map as Mp
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Word8 as W

import System.FilePath ((</>))

import qualified Parsing.Python as Py
import Generation.Utils (fieldNamed)


data SqlTable = SqlTable {
    nameST :: !Bs.ByteString
  , fieldsST :: ![SqlFieldDef]
  }
  deriving (Eq)

instance Show SqlTable where
  show table =
    "Table: " <> (T.unpack . T.decodeUtf8) table.nameST
    <> "\n" <> L.intercalate "\n  , " (map show table.fieldsST)


data SqlFieldDef = SqlFieldDef {
    nameSF :: !Bs.ByteString
  , kindSF :: !SqlFieldKind
  , params :: !SqlFieldParams
  , oriName :: !Bs.ByteString
  , required :: !Bool
  }
  deriving (Eq)

instance Show SqlFieldDef where
  show field =
    (T.unpack . T.decodeUtf8) field.nameSF <> " => " <> show field.kindSF <> " (" <> show field.params <> ")"

data SqlFieldKind =
  VarCharSFK
  | CharSFK
  | TextSFK
  | FullTextSFK
  | BooleanSFK
  | NumericSFK
  | IntegerSFK
  | FloatSFK
  | DateSFK
  | DateTimeSFK
  | TimeSFK
  | TimeDeltaSFK
  | TimestampSFK
  | EnumSFK
  | MultiValueSFK
  | SelectionSFK
  | MultiSelectionSFK
  | DictSFK
  | BinarySFK
  | RelationSFK SqlRelationKind
  | FunctionSFK
  | CommentSFK String
  deriving (Show, Eq)


data SqlRelationKind =
  Many2OneSFK
  | One2ManySFK
  | Many2ManySFK
  | One2OneSFK
  deriving (Show, Eq)

data SqlFieldParams =
  LengthPR Int
  | ForeignKeyPR Bs.ByteString Bs.ByteString
  | NonePR
  | CommentPR String
  deriving (Show, Eq)


genSchemas :: FilePath -> [SqlTable] -> IO (Mp.Map T.Text SqlTable)
genSchemas destPath sqlTables =
  let
    createDefs = map spitModel sqlTables
  in do
  Bs.writeFile (destPath </> "schemas.sql") (Bs.intercalate "\n" (map fst createDefs) <> Bs.intercalate "\n" (map snd createDefs))
  pure $ Mp.fromList [ (T.decodeUtf8 table.nameST, table) | table <- sqlTables ]


genTableDefs :: [Py.LogicElement] -> [Either String SqlTable]
genTableDefs elements =
  let
    sqlModels = foldr (\element accum ->
      case element of
        Py.ModelEl model ->
          case model.tClasses of
            Py.SqlTC -> model : accum
            Py.BothTC -> model : accum
            _ -> accum
        _ -> accum
      ) [] elements
    eiTableDefs = map parseModel sqlModels
  in
  eiTableDefs


parseModel :: Py.TrytonModel -> Either String SqlTable
parseModel model =
  let
    mbNameF = L.find (fieldNamed "__name__") model.fields
  in
  case mbNameF of
    Nothing -> Left $ "@[parseModel] model " <> model.name <> " has no __name__ field (" <> show model.fields <> ")"
    Just aField ->
      if Py.isStringLiteral aField.value then
        let
          eiFieldDefs = map trytonFieldToSql (Mp.elems model.fields)
          leftOvers = lefts eiFieldDefs
        in
        case leftOvers of
          [] -> Right (SqlTable {
              nameST = Bs.intercalate "." (Py.extractStringLiteral aField.value)
            , fieldsST = catMaybes $ rights eiFieldDefs
            })
          _ -> Left $ "@[parseModel] model " <> model.name <> " has left-over fields: " <> show leftOvers

      else
        Left $ "@[parseModel] model " <> model.name <> " has a non-string __name__ field"


spitModel :: SqlTable -> (Bs.ByteString, Bs.ByteString)
spitModel table =
  let
    tableName = modelToSqlName table.nameST
    fieldDefs = mapMaybe spitField table.fieldsST
    alterDefs = spitAlterDef tableName $ filterRefFields table.fieldsST
  in (
    "drop table if exists " <> tableName <> " cascade;\n"
    <> "create table " <> tableName <> " (\n"
    <> "   id serial primary key"
    <> (
      if null fieldDefs then
        ""
      else
        "\n  , " <> Bs.intercalate "\n  , " fieldDefs
      )
    <> "\n);\n"
    , Bs.intercalate "\n" alterDefs
  )
  where
  filterRefFields :: [SqlFieldDef] -> [SqlFieldDef]
  filterRefFields = filter (\field -> case field.kindSF of RelationSFK _ -> True; _ -> False)

  spitField :: SqlFieldDef -> Maybe Bs.ByteString
  spitField field =
    let
      mbFieldType =
        case field.kindSF of
          VarCharSFK ->
            case field.params of
                LengthPR len -> Just $ "varchar (" <> (T.encodeUtf8 . T.pack . show) len <> ")"
                _ -> Just "character varying"
          BooleanSFK ->
            Just "boolean"
          IntegerSFK ->
            Just "integer"
          FloatSFK ->
            Just "float"
          DateSFK ->
            Just "date"
          DateTimeSFK ->
            Just "timestamp"
          TimeSFK ->
            Just "time"
          TimeDeltaSFK ->
            Just "interval"
          TimestampSFK ->
            Just "timestamptz"
          BinarySFK ->
            Just "bytea"
          SelectionSFK ->
            Just "character varying"
          MultiSelectionSFK ->
            Just "character varying"
          EnumSFK ->
            Just "character varying"
          CharSFK ->
            Just "character varying"
          TextSFK ->
            Just "text"
          FullTextSFK ->
            Just "text"
          NumericSFK ->
            Just "numeric"
          RelationSFK relKind ->
            case relKind of
              Many2OneSFK -> Just "integer"
              One2ManySFK -> Nothing
              Many2ManySFK -> Just "integer"
              One2OneSFK -> Just "integer"
          FunctionSFK ->
            Nothing
          MultiValueSFK ->
            Just "text"
          DictSFK ->
            Just "text"
          CommentSFK _ ->
            Nothing
    in
    case mbFieldType of
      Nothing -> Nothing
      Just fType -> Just $ quoteReservedWord field.oriName <> " " <> fType <> if field.required then " not null" else ""


  spitAlterDef :: Bs.ByteString -> [SqlFieldDef] -> [Bs.ByteString]
  spitAlterDef tableName = map (\field ->
    let
        constraintName = tableName <> "_" <> field.oriName <> "_fkey"
        otherTable = case field.params of
          ForeignKeyPR otherTable _ -> Bs.intercalate "_" (Bs.split 46 otherTable)
          _ -> ""
        deleteClause =
          case field.kindSF of
            RelationSFK relKind ->
              case relKind of
                Many2OneSFK -> if field.required then "on delete restrict" else "on delete set null"
                One2ManySFK -> "on delete cascade"
                Many2ManySFK -> "on delete cascade"
                One2OneSFK -> if field.required then "on delete restrict" else "on delete set null"
            _ -> ""
      in
      "alter table " <> tableName
      <> "\n  add constraint " <> constraintName
      <> " foreign key (" <> field.oriName <> ") references " <> otherTable <> "(id) "
      <> deleteClause <> ";\n"
    )


trytonFieldToSql :: Py.Field -> Either String (Maybe SqlFieldDef)
trytonFieldToSql field =
  case field.name of
    "__name__" -> Right Nothing
    _ ->
      case field.value of
        Py.CallEx expr args ->
          case extractFieldDefs field.name expr args of
            Left err -> Left err
            Right fieldDef ->
              Right $ Just fieldDef
        Py.MapEx exprs ->
          Right Nothing
        _ -> Left $ "@[trytonFieldToSql] field " <> (T.unpack . T.decodeUtf8) field.name <> " is not a call, value: " <> show field.value


extractFieldDefs :: Bs.ByteString -> Py.Expr -> [Py.Argument] -> Either String SqlFieldDef
extractFieldDefs fieldName expr fArgs =
  case expr of
    Py.DotEx sExpr label ->
      case sExpr of
        Py.VarEx vName ->
          case vName of
            "fields" ->
              let
                context = "fields->" <> label <> ": "
              in
              trytonTypeToSql fieldName label fArgs
            "state" ->
              Right (SqlFieldDef { nameSF = "state" <> "->" <> label, kindSF = CommentSFK (show fArgs), params = NonePR, oriName = fieldName, required = False })
            _ ->
              Right (SqlFieldDef { nameSF = vName <> "->" <> label, kindSF = CommentSFK (show fArgs), params = NonePR, oriName = fieldName, required = False })
        _ -> Left $ "@[extractFieldDefs] expr is not a var: " <> show sExpr
    _ -> Left $ "@[extractFieldDefs] expr is not a call: " <> show expr


trytonTypeToSql :: Bs.ByteString -> Bs.ByteString -> [Py.Argument] -> Either String SqlFieldDef
trytonTypeToSql fieldName label fArgs =
  case label of
    "Selection" -> tmpBuildType fieldName label fArgs
    "MultiSelection" -> tmpBuildType fieldName label fArgs
    "Reference" -> tmpBuildType fieldName label fArgs
    "Many2One" -> buildMany2One fieldName fArgs
    "One2Many" -> tmpBuildType fieldName label fArgs
    "Many2Many" -> tmpBuildType fieldName label fArgs
    "One2One" -> tmpBuildType fieldName label fArgs
    "Function" -> tmpBuildType fieldName label fArgs
    "MultiValue" -> tmpBuildType fieldName label fArgs
    "Dict" -> tmpBuildType fieldName label fArgs
    _ -> buildColumn fieldName label fArgs

tmpBuildType :: Bs.ByteString -> Bs.ByteString -> [Py.Argument] -> Either String SqlFieldDef
tmpBuildType fieldName label fArgs =
  Right (SqlFieldDef { nameSF = "TMP:" <> fieldName, kindSF = CommentSFK (show fArgs), params = NonePR, oriName = fieldName, required = False })

buildOne2Many :: [Py.Argument] -> Either String SqlFieldDef
buildOne2Many fArgs =
  Left "@[buildOne2Many] unimplemented."

buildFunction :: [Py.Argument] -> Either String SqlFieldDef
buildFunction fArgs =
  Left "@[buildFunction] unimplemented."

buildSelection :: [Py.Argument] -> Either String SqlFieldDef
buildSelection fArgs =
  Left "@[buildSelection] unimplemented."


buildMany2One :: Bs.ByteString -> [Py.Argument] -> Either String SqlFieldDef
buildMany2One fieldName fArgs =
  case fArgs of
    [] -> Left "@[buildMany2One] no arg."
    [ a ] -> Left "@[buildMany2One] only one arg"
    a : b : mods ->
      case getLambdaArgString a of
        Just str1 ->
          case getLambdaArgString b of
            Just str2 ->
              let
                joinedStr1 = Bs.intercalate "." str1
                joinedStr2 = Bs.map (\c -> if c == 32 then 95 else W.toUpper c) $ Bs.intercalate "." str2
                requiredF = hasRequiredField mods
              in
              -- TODO: get the non-null flag from mods args:
              Right (SqlFieldDef { nameSF = joinedStr2 <> "_FK", kindSF = RelationSFK Many2OneSFK, params = ForeignKeyPR joinedStr1 joinedStr2, oriName = fieldName, required = requiredF })
            Nothing ->
              Left "@[buildMany2One] second arg is not a lambda arg"
        Nothing ->
          Left "@[buildMany2One] first arg is not a lambda arg"


hasRequiredField :: [Py.Argument] -> Bool
hasRequiredField = any (\arg ->
  case arg of
    Py.NamedArg varName expr ->
      varName == "required"
      && case expr of
            Py.LiteralEx (Py.BoolLit b) -> b
            _ -> False
    _ -> False
  )


getLambdaArgString :: Py.Argument -> Maybe [Bs.ByteString]
getLambdaArgString arg =
  case arg of
    Py.LambdaArg (Py.LiteralEx (Py.StringLit str)) -> Just $ map Py.removeQuotes str
    _ -> Nothing

{-
Tryton defines the following fields types:
  Boolean
  Integer
  Char
  Text
  FullText
  Float
  Numeric
  Date
  DateTime
  Timestamp
  Time
  TimeDelta
  Binary
  Selection
  MultiSelection
  Reference
  Many2One
  One2Many
  Many2Many
  One2One
  Function
  MultiValue
  Dict
-}

buildColumn :: Bs.ByteString -> Bs.ByteString -> [Py.Argument] -> Either String SqlFieldDef
buildColumn fieldName typeName fArgs =
  let
    fieldType = case typeName of
        "Boolean" -> Right BooleanSFK
        "Integer" -> Right IntegerSFK
        "Char" -> Right CharSFK
        "Date" -> Right DateSFK
        "Text" -> Right TextSFK
        "FullText" -> Right FullTextSFK
        "Float" -> Right FloatSFK
        "Numeric" -> Right NumericSFK
        "DateTime" -> Right DateTimeSFK
        "Timestamp" -> Right TimestampSFK
        "Time" -> Right TimeSFK
        "TimeDelta" -> Right TimeDeltaSFK
        "Binary" -> Right BinarySFK
        _ -> Left . (T.unpack . T.decodeUtf8) $ "@[buildColumn] unimplemented: " <> typeName
  in
  case fieldType of
    Left err -> Left err
    Right kind ->
      case fArgs of
        [] -> Left "@[extractFieldDefs] no expressions."
        [ a ] ->
          case a of
            Py.LambdaArg lExpr ->
              case lExpr of
                Py.LiteralEx (Py.StringLit str) ->
                  Right (SqlFieldDef { nameSF = Bs.intercalate "_" (map (Bs.map W.toUpper . Py.removeQuotes) str)
                            , kindSF = kind, params = NonePR, oriName = quoteReservedWord fieldName, required = False })
                _ -> Left $ "@[extractFieldDefs] unexpected lambda arg expr: " <> show lExpr
            _ -> Left $ "@[extractFieldDefs] unexpected first arg: " <> show a
        a : b : restArgs ->
          let
            requiredF = hasRequiredField restArgs
          in
          case a of
            Py.VarArg firstName ->
              Right (SqlFieldDef { nameSF = firstName, kindSF = kind, params = CommentPR (show restArgs), oriName = fieldName, required = requiredF })
            Py.LambdaArg lExpr ->
              case lExpr of
                Py.CallEx lcExpr lcArgs ->
                  case lcExpr of
                    Py.VarEx lcvName ->
                      Right (SqlFieldDef { nameSF = lcvName, kindSF = kind, params = CommentPR (show restArgs), oriName = fieldName, required = requiredF })
                    Py.DotEx ldExpr label ->
                      case ldExpr of
                        Py.VarEx lcdName ->
                          Right (SqlFieldDef { nameSF = lcdName, kindSF = kind, params = CommentPR (show restArgs), oriName = fieldName, required = requiredF })
                        _ -> Left $ "@[extractFieldDefs] unexpted lambdaArg.call.dot expr: " <> show ldExpr
                    _ -> Left $ "@[extractFieldDefs] unexpected lambdaArg.call expr: " <> show lcExpr
                  {--
                  Py.VarEx name ->
                    Right (SqlFieldDef { nameSF = name, kindSF = CommentFK (show fArgs) })
                  Py.LiteralEx (Py.StringLit str) ->
                    Right (SqlFieldDef { nameSF = str, kindSF = CommentFK (show fArgs) })
                  _ -> Left $ "@[extractFieldDefs] lambda expression is not a string literal: " <> show lExpr
                  --}
                Py.LiteralEx (Py.StringLit str) ->
                  Right (SqlFieldDef { nameSF = Bs.intercalate "_" (map (Bs.map W.toUpper . Py.removeQuotes) str)
                            , kindSF = kind, params = CommentPR (show restArgs)
                            , oriName = fieldName, required = requiredF })
                Py.ArrayEx exprs ->
                  Right (SqlFieldDef { nameSF = "[" <> Bs.intercalate "," (map Py.bsShowExpr exprs) <> "]", kindSF = kind
                            , params = CommentPR (show restArgs), oriName = fieldName, required = requiredF })
                _ -> Left $ "@[extractFieldDefs] unexpected lambda arg expr: " <> show lExpr
            _ -> Left $ "@[extractFieldDefs] unexpected first arg: " <> show a


modelToSqlName :: Bs.ByteString -> Bs.ByteString
modelToSqlName = Bs.intercalate "_" . concatMap (Bs.split 45) . Bs.split 46


quoteReservedWord :: Bs.ByteString -> Bs.ByteString
quoteReservedWord aWord =
  if isSqlReserved aWord then
    "\"" <> aWord <> "\""
  else
    aWord


isSqlReserved :: Bs.ByteString -> Bool
isSqlReserved aWord = Bs.map W.toUpper aWord `elem` [
      "ABS", "ABSENT", "ABSOLUTE", "ACOS", "ACTION", "ADD", "ALL", "ALLOCATE", "ALTER"
    , "ANALYSE", "ANALYZE", "AND", "ANY", "ANY_VALUE", "ARE", "ARRAY", "ARRAY_AGG"
    , "ARRAY_MAX_CARDINALITY", "AS", "ASC", "ASENSITIVE", "ASIN", "ASSERTION", "ASYMMETRIC"
    , "AT", "ATAN", "ATOMIC", "AUTHORIZATION", "AVG", "BEGIN", "BEGIN_FRAME", "BEGIN_PARTITION"
    , "BETWEEN", "BIGINT", "BINARY", "BIT", "BIT_LENGTH", "BLOB", "BOOLEAN", "BOTH", "BTRIM", "BY"
    , "CALL", "CALLED", "CARDINALITY", "CASCADE", "CASCADED", "CASE", "CAST", "CATALOG", "CEIL"
    , "CEILING", "CHAR", "CHARACTER", "CHARACTER_LENGTH", "CHAR_LENGTH", "CHECK", "CLASSIFIER", "CLOB"
    , "CLOSE", "COALESCE", "COLLATE", "COLLATION", "COLLECT", "COLUMN", "COMMIT", "CONCURRENTLY"
    , "CONDITION", "CONNECT", "CONNECTION", "CONSTRAINT", "CONSTRAINTS", "CONTAINS", "CONTINUE", "CONVERT"
    , "COPY", "CORR", "CORRESPONDING", "COS", "COSH", "COUNT", "COVAR_POP", "COVAR_SAMP", "CREATE", "CROSS"
    , "CUBE", "CUME_DIST", "CURRENT", "CURRENT_CATALOG", "CURRENT_DATE", "CURRENT_DEFAULT_TRANSFORM_GROUP"
    , "CURRENT_PATH", "CURRENT_ROLE", "CURRENT_ROW", "CURRENT_SCHEMA", "CURRENT_TIME", "CURRENT_TIMESTAMP"
    , "CURRENT_TRANSFORM_GROUP_FOR_TYPE", "CURRENT_USER", "CURSOR", "CYCLE", "DATALINK", "DATE", "DAY"
    , "DEALLOCATE", "DEC", "DECFLOAT", "DECIMAL", "DECLARE", "DEFAULT", "DEFERRABLE", "DEFERRED", "DEFINE"
    , "DELETE", "DENSE_RANK", "DEREF", "DESC", "DESCRIBE", "DESCRIPTOR", "DETERMINISTIC", "DIAGNOSTICS"
    , "DISCONNECT", "DISTINCT", "DLNEWCOPY", "DLPREVIOUSCOPY", "DLURLCOMPLETE", "DLURLCOMPLETEONLY"
    , "DLURLCOMPLETEWRITE", "DLURLPATH", "DLURLPATHONLY", "DLURLPATHWRITE", "DLURLSCHEME", "DLURLSERVER"
    , "DLVALUE", "DO", "DOMAIN", "DOUBLE", "DROP", "DYNAMIC", "EACH", "ELEMENT", "ELSE", "EMPTY", "END"
    , "END-EXEC", "END_FRAME", "END_PARTITION", "EQUALS", "ESCAPE", "EVERY", "EXCEPT", "EXCEPTION", "EXEC"
    , "EXECUTE", "EXISTS", "EXP", "EXTERNAL", "EXTRACT", "FALSE", "FETCH", "FILTER", "FIRST", "FIRST_VALUE"
    , "FLOAT", "FLOOR", "FOR", "FOREIGN", "FOUND", "FRAME_ROW", "FREE", "FREEZE", "FROM", "FULL", "FUNCTION"
    , "FUSION", "GET", "GLOBAL", "GO", "GOTO", "GRANT", "GREATEST", "GROUP", "GROUPING", "GROUPS", "HAVING"
    , "HOLD", "HOUR", "IDENTITY", "ILIKE", "IMMEDIATE", "IMPORT", "IN", "INDICATOR", "INITIAL", "INITIALLY"
    , "INNER", "INOUT", "INPUT", "INSENSITIVE", "INSERT", "INT", "INTEGER", "INTERSECT", "INTERSECTION"
    , "INTERVAL", "INTO", "IS", "ISNULL", "ISOLATION", "JOIN", "JSON", "JSON_ARRAY", "JSON_ARRAYAGG", "JSON_EXISTS"
    , "JSON_OBJECT", "JSON_OBJECTAGG", "JSON_QUERY", "JSON_SCALAR", "JSON_SERIALIZE", "JSON_TABLE"
    , "JSON_TABLE_PRIMITIVE", "JSON_VALUE", "KEY", "LAG", "LANGUAGE", "LARGE", "LAST", "LAST_VALUE", "LATERAL"
    , "LEAD", "LEADING", "LEAST", "LEFT", "LEVEL", "LIKE", "LIKE_REGEX", "LIMIT", "LISTAGG", "LN", "LOCAL"
    , "LOCALTIME", "LOCALTIMESTAMP", "LOG", "LOG10", "LOWER", "LPAD", "LTRIM", "MATCH", "MATCHES", "MATCH_NUMBER"
    , "MATCH_RECOGNIZE", "MAX", "MEMBER", "MERGE", "METHOD", "MIN", "MINUTE", "MOD", "MODIFIES", "MODULE", "MONTH"
    , "MULTISET", "NAMES", "NATIONAL", "NATURAL", "NCHAR", "NCLOB", "NEW", "NEXT", "NO", "NONE", "NORMALIZE", "NOT"
    , "NOTNULL", "NTH_VALUE", "NTILE", "NULL", "NULLIF", "NUMERIC", "OCCURRENCES_REGEX", "OCTET_LENGTH", "OF"
    , "OFFSET", "OLD", "OMIT", "ON", "ONE", "ONLY", "OPEN", "OPTION", "OR", "ORDER", "OUT", "OUTER", "OUTPUT"
    , "OVER", "OVERLAPS", "OVERLAY", "PAD", "PARAMETER", "PARTIAL", "PARTITION", "PATTERN", "PER", "PERCENT"
    , "PERCENTILE_CONT", "PERCENTILE_DISC", "PERCENT_RANK", "PERIOD", "PLACING", "PORTION", "POSITION"
    , "POSITION_REGEX", "POWER", "PRECEDES", "PRECISION", "PREPARE", "PRESERVE", "PRIMARY", "PRIOR", "PRIVILEGES"
    , "PROCEDURE", "PTF", "PUBLIC", "RANGE", "RANK", "READ", "READS", "REAL", "RECURSIVE", "REF", "REFERENCES"
    , "REFERENCING", "REGR_AVGX", "REGR_AVGY", "REGR_COUNT", "REGR_INTERCEPT", "REGR_R2", "REGR_SLOPE", "REGR_SXX"
    , "REGR_SXY", "REGR_SYY", "RELATIVE", "RELEASE", "RESTRICT", "RESULT", "RETURN", "RETURNING", "RETURNS"
    , "REVOKE", "RIGHT", "ROLLBACK", "ROLLUP", "ROW", "ROWS", "ROW_NUMBER", "RPAD", "RTRIM", "RUNNING", "SAVEPOINT"
    , "SCHEMA", "SCOPE", "SCROLL", "SEARCH", "SECOND", "SECTION", "SEEK", "SELECT", "SENSITIVE", "SESSION"
    , "SESSION_USER", "SET", "SHOW", "SIMILAR", "SIN", "SINH", "SIZE", "SKIP", "SMALLINT", "SOME", "SPACE"
    , "SPECIFIC", "SPECIFICTYPE", "SQL", "SQLCODE", "SQLERROR", "SQLEXCEPTION", "SQLSTATE", "SQLWARNING", "SQRT"
    , "START", "STATIC", "STDDEV_POP", "STDDEV_SAMP", "SUBMULTISET", "SUBSET", "SUBSTRING", "SUBSTRING_REGEX"
    , "SUCCEEDS", "SUM", "SYMMETRIC", "SYSTEM", "SYSTEM_TIME", "SYSTEM_USER", "TABLE", "TABLESAMPLE", "TAN"
    , "TANH", "TEMPORARY", "THEN", "TIME", "TIMESTAMP", "TIMEZONE_HOUR", "TIMEZONE_MINUTE", "TO", "TRAILING"
    , "TRANSACTION", "TRANSLATE", "TRANSLATE_REGEX", "TRANSLATION", "TREAT", "TRIGGER", "TRIM", "TRIM_ARRAY"
    , "TRUE", "TRUNCATE", "UESCAPE", "UNION", "UNIQUE", "UNKNOWN", "UNNEST", "UPDATE", "UPPER", "USAGE", "USER"
    , "USING", "VALUE", "VALUES", "VALUE_OF", "VARBINARY", "VARCHAR", "VARIADIC", "VARYING", "VAR_POP", "VAR_SAMP"
    , "VERBOSE", "VERSIONING", "VIEW", "WHEN", "WHENEVER", "WHERE", "WIDTH_BUCKET", "WINDOW", "WITH", "WITHIN"
    , "WITHOUT", "WORK", "WRITE", "XML", "XMLAGG", "XMLATTRIBUTES", "XMLBINARY", "XMLCAST", "XMLCOMMENT", "XMLCONCAT"
    , "XMLDOCUMENT", "XMLELEMENT", "XMLEXISTS", "XMLFOREST", "XMLITERATE", "XMLNAMESPACES", "XMLPARSE", "XMLPI"
    , "XMLQUERY", "XMLSERIALIZE", "XMLTABLE", "XMLTEXT", "XMLVALIDATE", "YEAR", "ZONE"
    ]