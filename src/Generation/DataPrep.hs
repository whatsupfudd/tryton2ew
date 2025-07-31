module Generation.DataPrep where

import qualified Data.ByteString as Bs
import Data.Either (rights, lefts)
import qualified Data.List as L
import qualified Data.Map as Mp
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import System.FilePath ((</>))

import qualified Generation.Sql as Sq
import qualified Tryton.Types as Tm


genBootstrap :: FilePath -> Mp.Map T.Text Sq.SqlTable -> Mp.Map T.Text [Tm.ClassInstance] -> IO ()
genBootstrap destDir sqlTables instanceMap =
  let
    eiDataPreps = map (genDataPrep sqlTables) (Mp.toList instanceMap)
  in
  case rights eiDataPreps of
    [] -> do
      putStrLn $ "@[genDataPrep] only errs: " <> L.intercalate "\n, " (lefts eiDataPreps)
      putStrLn $ "@[genDataPrep] parsed tables: " <> L.intercalate "\n, " (map T.unpack (Mp.keys sqlTables))
    dataPreps -> do
      Bs.writeFile (destDir </> "dataPrep.sql") (Bs.intercalate "\n\n" dataPreps)
      putStrLn $ "@[genDataPrep] parsed tables: " <> L.intercalate "\n, " (map T.unpack (Mp.keys sqlTables))
      putStrLn $ "@[genDataPrep] some errs: " <> L.intercalate "\n, " (lefts eiDataPreps)


genDataPrep :: Mp.Map T.Text Sq.SqlTable -> (T.Text, [Tm.ClassInstance]) -> Either String Bs.ByteString
genDataPrep sqlTables classInstances =
  let
    eiDataPreps = spitDataPrep sqlTables classInstances
  in
  case eiDataPreps of
    Left errs ->
      Left $ "@[genDataPrep] errs: " <> errs
    Right dataPreps -> do
      Right dataPreps


spitDataPrep :: Mp.Map T.Text Sq.SqlTable -> (T.Text, [Tm.ClassInstance]) -> Either String Bs.ByteString
spitDataPrep sqlTables (model, instances) =
  case Mp.lookup model sqlTables of
    Nothing -> Left $ "@[spitDataPrep] no table for model: " <> T.unpack model
    Just table ->
      let
        (fieldMap, requireds) = resolveFields table.fieldsST
        fieldNames = Bs.intercalate ", " [ aField.oriName | aField <- Mp.elems fieldMap]  -- get fields from table
        tablePrefix = "INSERT INTO " <> Sq.modelToSqlName table.nameST <> " (" <> fieldNames <> ") VALUES ("
      in
      let
        rezA = map (createInsertValues fieldMap requireds) instances
        errs = concatMap snd rezA
      in
      if null errs then
        Right $ Bs.intercalate "\n" (
            map (\(insValues, _) ->
                tablePrefix <> Bs.intercalate ", " insValues <> ");"
              ) rezA
          )
      else
        Left $ "@[spitDataPrep] table: " <> T.unpack model
          <> "; resolvedFields: " <> L.intercalate ", " [ T.unpack . T.decodeUtf8 $ aField.oriName | aField <- Mp.elems fieldMap]
          <> "; errs: " <> L.intercalate "\n, " errs


resolveFields :: [Sq.SqlFieldDef] -> (Mp.Map T.Text Sq.SqlFieldDef, [T.Text])
resolveFields =
  foldr (\aField (fMap, requireds) ->
    let
      updMap = Mp.insert (T.decodeUtf8 aField.oriName) aField fMap
      updReqs = if aField.required then T.decodeUtf8 aField.oriName : requireds else requireds
    in
    (updMap, updReqs)
  ) (Mp.empty, [])


createInsertValues :: Mp.Map T.Text Sq.SqlFieldDef -> [T.Text] -> Tm.ClassInstance -> ([Bs.ByteString], [String])
createInsertValues fieldMap requireds anInstance =
  let
    (resultStrs, reqLeftover, errList) =
      foldr (\(fName, fValue) (outStr, missingReqs, errs) ->
        case Mp.lookup fName fieldMap of
          Nothing -> (outStr, missingReqs, ("@[createInsertValues] unknown field: " <> T.unpack fName) : errs)
          Just fieldDef ->
            case fValue.kindF of
              Tm.LabelFK ->
                if fieldDef.kindSF `elem` [Sq.VarCharSFK,Sq.CharSFK, Sq.DateSFK, Sq.DateTimeSFK, Sq.TimeSFK, Sq.TimeDeltaSFK, Sq.TimestampSFK] then
                  ("\'" <> T.encodeUtf8 fValue.valueF <> "\'" : outStr, L.delete fName missingReqs, errs)
                else case fieldDef.kindSF of
                  Sq.IntegerSFK -> (T.encodeUtf8 fValue.valueF : outStr, L.delete fName missingReqs, errs)
                  Sq.FloatSFK -> (T.encodeUtf8 fValue.valueF : outStr, L.delete fName missingReqs, errs)
                  Sq.BooleanSFK ->
                    let
                      val = if T.toLower fValue.valueF == "true" then "true" else "false"
                    in
                    (val : outStr, L.delete fName missingReqs, errs)
                  _ -> (outStr, missingReqs
                        , ("@[createInsertValues] unsupported SQL kind: " <> show fieldDef.kindSF <> " for field: " <> T.unpack fName) : errs)
              Tm.EvalFK ->
                (outStr, L.delete fName missingReqs, "@[createInsertValues] unsupported Eval field: " <> T.unpack fName : errs)
              Tm.ReferenceFK ->
                (outStr, L.delete fName missingReqs, "@[createInsertValues] unsupported Reference field: " <> T.unpack fName : errs)
        ) ([], requireds, []) (Mp.toList anInstance.fieldsDF)
  in
  (resultStrs, errList)
