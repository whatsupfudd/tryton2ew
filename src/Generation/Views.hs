module Generation.Views where

import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Either (rights, lefts)

import qualified Parsing.Python as Py
import qualified Generation.Sql as Sq

import Generation.Utils (fieldNamed)


data HtmlView = HtmlView {
    nameHV :: !String
  , fieldsHV :: ![Sq.SqlFieldDef]
  }
  deriving (Eq)

instance Show HtmlView where
  show view =
    "create table " <> view.nameHV <> " (\n"
    <>
      L.intercalate ", " (map (\f -> show f <> "\n") view.fieldsHV)
    <> ")"


genViewDef :: [Py.LogicElement] -> [Either String HtmlView]
genViewDef elements =
  let
    sqlModels = foldr (\element accum ->
      case element of
        Py.ModelEl model ->
          case model.tClasses of
            Py.BothTC -> model : accum
            Py.ViewTC -> model : accum
            _ -> accum
        _ -> accum
      ) [] elements
    eiTableDefs = map parseModel sqlModels
  in
  eiTableDefs


parseModel :: Py.TrytonModel -> Either String HtmlView
parseModel model =
  let
    mbNameF = L.find (fieldNamed "v:__name__") model.fields
  in
  case mbNameF of
    Nothing -> Left $ "@[parseModel] model " <> model.name <> " has no __name__ field (" <> show model.fields <> ")"
    Just aField ->
      if Py.isStringLiteral aField.value then
        let
          eiFieldDefs = map Sq.trytonFieldToSql model.fields
          leftOvers = lefts eiFieldDefs
        in
        case leftOvers of
          [] -> Right (HtmlView {
              nameHV = L.intercalate "." (Py.extractStringLiteral aField.value)
            , fieldsHV = catMaybes $ rights eiFieldDefs
            })
          _ -> Left $ "@[parseModel] model " <> model.name <> " has left-over fields: " <> show leftOvers

      else
        Left $ "@[parseModel] model " <> model.name <> " has a non-string __name__ field"
