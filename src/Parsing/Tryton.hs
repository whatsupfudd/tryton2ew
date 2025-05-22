{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Parsing.Tryton where

import Control.Applicative         ((<|>))
import Control.Monad               (forM, forM_, mapM)
import Control.Exception           (try, SomeException)

import qualified Data.Map.Strict as Mp
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import Text.XML (Document(..), Element(..), Node(..), Prologue(..), renderText)
import qualified Text.XML as X
import Text.XML.Cursor (Cursor, attribute, child, content
                        , element, fromDocument, node, ($//), ($/), (>=>), (&/))



data MenuItem = MenuItem {
    idMI       :: !T.Text
  , parentMI   :: !(Maybe T.Text)
  , nameMI     :: !(Maybe T.Text)
  , iconMI     :: !(Maybe T.Text)
  , childrenMI :: ![MenuItem]
  }
  deriving (Show, Eq)


data Definition = Definition { 
    idDF     :: !T.Text
  , modelDF  :: !T.Text
  , fieldsDF :: !(Mp.Map T.Text Field)
  }
  deriving (Show, Eq)

data FieldKind =
  LabelFK
  | EvalFK
  | ReferenceFK
  deriving (Show, Eq)

data Field = Field {
    kindF :: !FieldKind
  , valueF :: !T.Text
  }
  deriving (Show, Eq)


extractItems :: Document -> [MenuItem]
extractItems doc =
    let cursor = fromDocument doc
        dataNodes = cursor $// element "data"
        items = concatMap (\d -> d $/ element "menuitem") dataNodes
    in do
    map parseItem items
  where
    parseItem c =
      let
        mid = case attribute "id" c of
                [i] -> i
                _ -> error "menuitem missing or multiple ids"
        parent = listToMaybe $ attribute "parent" c
        nameAttr = listToMaybe (attribute "name" c)
        labelTxt = listToMaybe (c $/ element "label" &/ content)
        iconAttr = listToMaybe (attribute "icon" c)
        nameVal = nameAttr <|> labelTxt
      in
      MenuItem {
        idMI = mid, parentMI = parent, nameMI = nameVal, iconMI = iconAttr, childrenMI = []
      }


extractDefinitions :: Document -> [Definition]
extractDefinitions doc =
  let
    cur = fromDocument doc
    dataNodes = cur $// element "data"
    !recs = concatMap (\d -> d $/ element "record") dataNodes
  in
  map parseRec recs
  where
  parseRec c =
    let
      !rid = singleAttr "id" c
      !model = singleAttr "model" c
      !fields = Mp.fromList [
                  (singleAttr "name" field, parseField field) | field <- c $/ element "field"
                ]
    in
    Definition {
      idDF = rid
      , modelDF = model
      , fieldsDF = fields
    }
  singleAttr :: X.Name -> Cursor -> T.Text
  singleAttr !n !c = case attribute n c of
                      [v] -> v
                      _   -> error $ "Expected single '" ++ T.unpack n.nameLocalName ++ "' attribute"


parseField :: Cursor -> Field
parseField !c =
  case attribute "eval" c of
    [v] -> Field {kindF = EvalFK, valueF = v}
    _ -> case attribute "ref" c of
          [v] -> Field {kindF = ReferenceFK, valueF = v}
          _   -> Field {kindF = LabelFK, valueF = getContent c}


getContent :: Cursor -> T.Text
getContent !cursor =
  case node cursor of
    NodeElement element ->
      let
        !children = X.elementNodes element
      in
      T.intercalate "," $ concatMap (\case
            NodeContent t -> [t]
            _ -> []
        ) children
    _ -> ""


buildTree :: [MenuItem] -> [MenuItem]
buildTree !items =
  let
    !allIds = Set.fromList (map idMI items)
    !childrenMap = Mp.fromListWith (++) [ (p, [i]) | i <- items, p <- maybeToList i.parentMI ]
    attachChildren !i =
        let
          !kids = Mp.findWithDefault [] (idMI i) childrenMap
        in
        i { childrenMI = map attachChildren kids }
    isRoot :: MenuItem -> Bool
    isRoot !i = case i.parentMI of
        Just p  -> not (Set.member p allIds)
        Nothing -> True
    roots = [ attachChildren i | i <- items, isRoot i ]
    in roots


printTree :: [MenuItem] -> Int -> FilePath -> IO ()
printTree !items !depth !destPath =
  TIO.writeFile (destPath </> "tree.txt") . T.intercalate "\n" $
    printTree' items depth
  where
  printTree' :: [MenuItem] -> Int -> [T.Text]
  printTree' !items !depth =
    let
      !offset = T.replicate (depth * 2) " "
    in
    map (\i ->
      let
        !label = maybe ("id=" <> i.idMI) ("menu: " <>) i.nameMI
        !parent = ("p=" <>) <$> i.parentMI
        !icon = ("i=" <>) <$> i.iconMI
        !piPart = T.intercalate ", " (maybeToList parent <> maybeToList icon)
        !details = " [id=" <> i.idMI <> if piPart == "" then "" else (", " <> piPart) <> "]"
        prefix = case depth of
            0 -> label <> details
            _ -> offset <> label <> maybe "" (\i -> " [" <> i <> "]") icon
      in
      prefix <> "\n" <> T.intercalate "\n" (printTree' i.childrenMI (depth + 1))
    ) items


printDefinitions :: Mp.Map T.Text [Definition] -> FilePath -> IO ()
printDefinitions !modelDefs !destPath =
  TIO.writeFile (destPath </> "definitions.txt") . T.intercalate "\n" $
    map (\(modelName, defs) ->
        "-- model: " <> modelName <> " --\n"
        <> T.intercalate "\n" (map (\d ->
              d.idDF <> ":\n"
              <> T.intercalate "\n" (map (\(k,v) ->
                    "  - " <> k <> ": " <> showField v
                  ) (Mp.toList (fieldsDF d)))
            ) defs)
      ) (Mp.toList modelDefs)


showField :: Field -> T.Text
showField !f = case f.kindF of
  LabelFK -> f.valueF
  EvalFK -> "#" <> f.valueF
  ReferenceFK -> "@" <> f.valueF
