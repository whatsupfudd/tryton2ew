module Generation.Sql where

import qualified Data.Char as C
import Data.Either (rights, lefts)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import qualified Data.Map as Mp
import qualified Data.Text as T

import qualified Parsing.Python as Py
import Generation.Utils (fieldNamed)


data SqlTable = SqlTable {
    nameST :: !String
  , fieldsST :: ![SqlFieldDef]
  }
  deriving (Eq)

instance Show SqlTable where
  show table =
    "create table " <> table.nameST <> " (\n"
    <>
      L.intercalate ", " (map (\f -> show f <> "\n") table.fieldsST)
    <> ")"

data SqlFieldDef = SqlFieldDef {
    nameSF :: !String
  , kindSF :: !SqlFieldKind
  , params :: !SqlFieldParams
  , oriName :: !T.Text
  }
  deriving (Eq)

instance Show SqlFieldDef where
  show field =
    field.nameSF <> " => " <> show field.kindSF <> " " <> show field.params

data SqlFieldKind =
  VarCharSFK
  | BooleanSFK
  | IntegerSFK
  | FloatSFK
  | DateSFK
  | DateTimeSFK
  | TimeSFK
  | TimeDeltaSFK
  | TimestampSFK
  | BinarySFK
  | SelectionSFK
  | MultiSelectionSFK
  | EnumSFK
  | ReferenceSFK
  | CharSFK
  | TextSFK
  | FullTextSFK
  | NumericSFK
  | Many2OneSFK
  | One2ManySFK
  | Many2ManySFK
  | One2OneSFK
  | FunctionSFK
  | MultiValueSFK
  | DictSFK
  | CommentSFK String
  deriving (Show, Eq)

data SqlFieldParams =
  LengthPR Int
  | ForeignKeyPR String String
  | NonePR
  | CommentPR String
  deriving (Show, Eq)


genTableDef :: [Py.LogicElement] -> [Either String SqlTable]
genTableDef elements =
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
    mbNameF = L.find (fieldNamed "v:__name__") model.fields
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
              nameST = L.intercalate "." (Py.extractStringLiteral aField.value)
            , fieldsST = catMaybes $ rights eiFieldDefs
            })
          _ -> Left $ "@[parseModel] model " <> model.name <> " has left-over fields: " <> show leftOvers

      else
        Left $ "@[parseModel] model " <> model.name <> " has a non-string __name__ field"


trytonFieldToSql :: Py.Field -> Either String (Maybe SqlFieldDef)
trytonFieldToSql field =
  case field.name of
    "v:__name__" -> Right Nothing
    _ ->
      case field.value of
        Py.CallEx expr args ->
          case extractFieldDefs field.name expr args of
            Left err -> Left err
            Right fieldDef -> 
              Right $ Just fieldDef
        Py.MapEx exprs ->
          Right Nothing
        _ -> Left $ "@[trytonFieldToSql] field " <> T.unpack field.name <> " is not a call, value: " <> show field.value


extractFieldDefs :: T.Text -> Py.Expr -> [Py.Argument] -> Either String SqlFieldDef
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
              Right (SqlFieldDef { nameSF = "state" <> "->" <> label, kindSF = CommentSFK (show fArgs), params = NonePR, oriName = fieldName })
            _ ->
              Right (SqlFieldDef { nameSF = vName <> "->" <> label, kindSF = CommentSFK (show fArgs), params = NonePR, oriName = fieldName })
        _ -> Left $ "@[extractFieldDefs] expr is not a var: " <> show sExpr
    _ -> Left $ "@[extractFieldDefs] expr is not a call: " <> show expr


trytonTypeToSql :: T.Text -> String -> [Py.Argument] -> Either String SqlFieldDef
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

tmpBuildType :: T.Text -> String -> [Py.Argument] -> Either String SqlFieldDef
tmpBuildType fieldName label fArgs =
  Right (SqlFieldDef { nameSF = "TMP:" <>label, kindSF = CommentSFK (show fArgs), params = NonePR, oriName = fieldName })

buildOne2Many :: [Py.Argument] -> Either String SqlFieldDef
buildOne2Many fArgs =
  Left "@[buildOne2Many] unimplemented."

buildFunction :: [Py.Argument] -> Either String SqlFieldDef
buildFunction fArgs =
  Left "@[buildFunction] unimplemented."

buildSelection :: [Py.Argument] -> Either String SqlFieldDef
buildSelection fArgs =
  Left "@[buildSelection] unimplemented."


buildMany2One :: T.Text -> [Py.Argument] -> Either String SqlFieldDef
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
                joinedStr1 = L.intercalate "." str1
                joinedStr2 = map (\c -> C.toUpper $ if c == ' ' then '_' else c) $ L.intercalate "." str2
              in
              -- TODO: get the non-null flag from mods args:
              Right (SqlFieldDef { nameSF = joinedStr2 <> "_FK", kindSF = ReferenceSFK, params = ForeignKeyPR joinedStr1 joinedStr2, oriName = fieldName })
            Nothing ->
              Left "@[buildMany2One] second arg is not a lambda arg"
        Nothing ->
          Left "@[buildMany2One] first arg is not a lambda arg"


getLambdaArgString :: Py.Argument -> Maybe [String]
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

buildColumn :: T.Text -> String -> [Py.Argument] -> Either String SqlFieldDef
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
        _ -> Left $ "@[buildColumn] unimplemented: " <> typeName
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
                  Right (SqlFieldDef { nameSF = L.intercalate "_" (map (map C.toUpper . Py.removeQuotes) str)
                            , kindSF = kind, params = NonePR, oriName = fieldName })
                _ -> Left $ "@[extractFieldDefs] unexpected lambda arg expr: " <> show lExpr
            _ -> Left $ "@[extractFieldDefs] unexpected first arg: " <> show a
        a : b : restArgs ->
          case a of
            Py.VarArg firstName ->
              Right (SqlFieldDef { nameSF = firstName, kindSF = kind, params = CommentPR (show restArgs), oriName = fieldName })
            Py.LambdaArg lExpr ->
              case lExpr of
                Py.CallEx lcExpr lcArgs ->
                  case lcExpr of
                    Py.VarEx lcvName ->
                      Right (SqlFieldDef { nameSF = lcvName, kindSF = kind, params = CommentPR (show restArgs), oriName = fieldName })
                    Py.DotEx ldExpr label ->
                      case ldExpr of
                        Py.VarEx lcdName ->
                          Right (SqlFieldDef { nameSF = lcdName, kindSF = kind, params = CommentPR (show restArgs), oriName = fieldName })
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
                  Right (SqlFieldDef { nameSF = L.intercalate "_" (map (map C.toUpper . Py.removeQuotes) str)
                            , kindSF = kind, params = CommentPR (show restArgs)
                            , oriName = fieldName })
                Py.ArrayEx exprs ->
                  Right (SqlFieldDef { nameSF = show exprs, kindSF = kind
                            , params = CommentPR (show restArgs), oriName = fieldName })
                _ -> Left $ "@[extractFieldDefs] unexpected lambda arg expr: " <> show lExpr
            _ -> Left $ "@[extractFieldDefs] unexpected first arg: " <> show a
