{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Parsing.Xml where

import Control.Applicative         ((<|>))
import Control.Monad               (forM, forM_, mapM)
import Control.Exception           (try, SomeException)

import qualified Data.Either as Ei
import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeFileName,takeExtension, (</>)
        , takeBaseName, splitDirectories)

import Text.XML (Document(..), Element(..), Node(..), Prologue(..), renderText)
import qualified Text.XML as X
import Text.XML.Cursor (Cursor, attribute, child, content
                        , element, fromDocument, node, ($//), ($/), (>=>), (&/), ($|), anyElement, fromNode)


import qualified Tryton.Types as Tm


-- Used for active windows docs:
extractMenuItems :: Document -> [Tm.MenuItem]
extractMenuItems doc =
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
        (parent, namespace) = maybe (Nothing, Nothing) extractMenuParent $ listToMaybe $ attribute "parent" c
        nameAttr = listToMaybe (attribute "name" c)
        labelTxt = listToMaybe (c $/ element "label" &/ content)
        iconAttr = listToMaybe (attribute "icon" c)
        sequence = maybe 10000000 (read . T.unpack) (listToMaybe (attribute "sequence" c))
        action = listToMaybe (attribute "action" c)
        nameVal = nameAttr <|> labelTxt
      in
      Tm.MenuItem {
        idMI = mid
        , parentMI = parent
        , namespaceMI = namespace
        , nameMI = nameVal
        , iconMI = iconAttr
        , seqOrdMI = sequence
        , childrenMI = []
        , actionMI = action
      }


extractMenuParent :: T.Text -> (Maybe T.Text, Maybe T.Text)
extractMenuParent parent =
  case T.breakOn "." parent of
    ("", "") -> (Nothing, Nothing)
    (aParent, "") -> (Just aParent, Nothing)
    (aNamespace, aParent) -> (Just $ T.drop 1 aParent, Just aNamespace)

parseModelView :: Document -> [Tm.Definition]
parseModelView doc =
  let
    cur = fromDocument doc
    dataNodes = cur $// element "data"
    !recs = concatMap (\d -> d $/ element "record") dataNodes
  in
  map parseRec recs


parseUiView :: FilePath -> Document -> ([Tm.Definition], T.Text)
parseUiView fileName doc =
  let
    nameParts = T.splitOn "_" . T.pack $ fileName
    rez =
      case last nameParts of
        "tree" ->
          parseTargetElement doc "tree" parseTree
        "list" ->  -- Used in the Tryton documentation for trees.
          parseTargetElement doc "tree" parseTree
        "form" ->
          case parseTargetElement doc "form" parseForm of
            [ Tm.ParseErrorDF err ] ->
              parseTargetElement doc "data" parseForm
            aRez -> aRez
        "list-form" ->
          parseTargetElement doc "list-form" parseTmpFunc
        "graph" ->
          parseTargetElement doc "graph" parseTmpFunc
        "board" ->
          parseTargetElement doc "board" parseTmpFunc
        "calendar" ->
          parseTargetElement doc "calendar" parseTmpFunc
        _ ->
          parseTargetElement doc "form" parseForm
          -- [ ParseErrorDF $ "@[extractDefinitions] invalid element: " <> T.unpack (last nameParts) ]
    in
    (rez, last nameParts)

-- Obsolete:
extractDefinitions :: FilePath -> Document -> ([Tm.Definition], (String, [String], [T.Text]))
extractDefinitions filePath doc =
  let
    dirPath = splitDirectories . takeDirectory $ filePath
    fileName = takeBaseName filePath
    nameParts = T.splitOn "_" . T.pack $ fileName
  in
  if last dirPath == "view" then
    let
      rez =
        case last nameParts of
          "tree" ->
            parseTargetElement doc "tree" parseTree
          "list" ->  -- Used in the Tryton documentation for trees.
            parseTargetElement doc "tree" parseTree
          "form" ->
            case parseTargetElement doc "form" parseForm of
              [ Tm.ParseErrorDF err ] ->
                parseTargetElement doc "data" parseForm
              aRez -> aRez
          "list-form" ->
            parseTargetElement doc "list-form" parseTmpFunc
          "graph" ->
            parseTargetElement doc "graph" parseTmpFunc
          "board" ->
            parseTargetElement doc "board" parseTmpFunc
          "calendar" ->
            parseTargetElement doc "calendar" parseTmpFunc
          _ ->
            parseTargetElement doc "form" parseForm
            -- [ ParseErrorDF $ "@[extractDefinitions] invalid element: " <> T.unpack (last nameParts) ]
    in
    (rez, (fileName, [last dirPath], [last nameParts]))
  else
    let
      cur = fromDocument doc
      dataNodes = cur $// element "data"
      !recs = concatMap (\d -> d $/ element "record") dataNodes
    in
    (map parseRec recs, (fileName, dirPath, nameParts))

parseRec c =
  let
    !rid = singleAttr "id" c
    !model = singleAttr "model" c
    !fields = Mp.fromList [
                (singleAttr "name" field, parseField field) | field <- c $/ element "field"
              ]
  in
  Tm.ModelDF $ Tm.ModelInstance {
    idDF = rid
    , modelDF = model
    , fieldsDF = fields
  }
singleAttr :: X.Name -> Cursor -> T.Text
singleAttr !n !c = case attribute n c of
                    [v] -> v
                    _   -> error $ "Expected single '" ++ T.unpack n.nameLocalName ++ "' attribute"

parseTargetElement :: Document -> X.Name -> (Cursor -> Either String Tm.Definition) -> [Tm.Definition]
parseTargetElement aDoc targetName parseFunc =
  let
    cursor = fromDocument aDoc
    targetNodes = cursor $| element targetName
    eiParseRez = map parseFunc targetNodes
  in
  if null (Ei.lefts eiParseRez) then
    if null (Ei.rights eiParseRez) then
      [ Tm.ParseErrorDF $ "@[parseTargetElement] no " <> T.unpack targetName.nameLocalName <> " found, " <> show targetNodes
        , Tm.ParseErrorDF $ "@[parseTargetElement] top element: " <> show (case node cursor of NodeElement anElement -> anElement.elementName.nameLocalName; _ -> "unknown")
      ]
    else
      Ei.rights eiParseRez
  else
    [ Tm.ParseErrorDF err | err <- Ei.lefts eiParseRez ]


parseTree :: Cursor -> Either String Tm.Definition
parseTree c =
  case node c of
    NodeElement anElement ->
      case anElement.elementName of
        "tree" ->
          let
            subNodes = concatMap (anyElement . fromNode) anElement.elementNodes
            attributes = map (\(k,v) ->
                      Tm.Attribute { nameA = k.nameLocalName, valueA = v }
                  ) (Mp.toList anElement.elementAttributes)
            children = map parseTreeElement subNodes
          in
          if null (Ei.lefts children) then
            Right $ Tm.TreeDF attributes (Ei.rights children)
          else
            Left $ "@[parseTree] errs: " <> L.intercalate "; " (Ei.lefts children)
        anotherName -> Left $ "@[parseTree] invalid element: " <> T.unpack anotherName.nameLocalName
    anotherNode -> Left $ "@[parseTree] didn't get an element." <> show anotherNode
  where
  parseTreeElement :: Cursor -> Either String Tm.TreeElement
  parseTreeElement aCursor =
    case node aCursor of
      NodeElement anElement ->
        let
          attributes = map (\(k,v) ->
                      Tm.Attribute { nameA = k.nameLocalName, valueA = v }
                  ) (Mp.toList anElement.elementAttributes)
        in
        case anElement.elementName of
          "field" ->
            let
              subFixes = concatMap (anyElement . fromNode) anElement.elementNodes
              nfixes = map parseNFixElement subFixes
            in
            if null (Ei.lefts nfixes) then
              Right $ Tm.FieldTE attributes (Ei.rights nfixes)
            else
              Left $ "@[parseTreeElement] errs: " <> L.intercalate "; " (Ei.lefts nfixes)
          "button" -> Right $ Tm.ButtonTE attributes
          _ -> Left $ "@[parseTreeElement] invalid element: " <> T.unpack anElement.elementName.nameLocalName
      _ -> Left $ "@[parseTreeElement] not a NodeElement:" <> show (node aCursor)
  parseNFixElement :: Cursor -> Either String Tm.NFixElement
  parseNFixElement aCursor =
    case node aCursor of
      NodeElement anElement ->
        let
          attributes = map (\(k,v) ->
                      Tm.Attribute { nameA = k.nameLocalName, valueA = v }
                  ) (Mp.toList anElement.elementAttributes)
        in
        case anElement.elementName of
          "prefix" -> Right $ Tm.PrefixNF attributes
          "suffix" -> Right $ Tm.SuffixNF attributes
          _ -> Left $ "@[parseNFixElement] invalid element: " <> T.unpack anElement.elementName.nameLocalName
      _ -> Left $ "@[parseNFixElement] not a NodeElement:" <> show (node aCursor)


parseForm :: Cursor -> Either String Tm.Definition
parseForm aCursor =
  case node aCursor of
    NodeElement anElement ->
      case anElement.elementName of
        "form" ->
          let
            subNodes = concatMap (anyElement . fromNode) anElement.elementNodes
            attributes = map (\(k,v) ->
                      Tm.Attribute { nameA = k.nameLocalName, valueA = v }
                  ) (Mp.toList anElement.elementAttributes)
            children = map parseFormElement subNodes
          in
          if null (Ei.lefts children) then
            Right $ Tm.FormDF attributes (Ei.rights children)
          else
            Left $ "@[parseForm] errs: " <> L.intercalate "; " (Ei.lefts children)
        "data" -> Left "@[parseForm] unsupported <data> in _form file."
        anotherName -> Left $ "@[parseForm] invalid element: " <> T.unpack anotherName.nameLocalName
    anotherNode -> Left $ "@[parseForm] didn't get an element." <> show anotherNode
  where
  parseFormElement :: Cursor -> Either String Tm.FormElement
  parseFormElement aCursor =
    case node aCursor of
      NodeElement anElement ->
        let
          attributes = map (\(k,v) ->
                      Tm.Attribute { nameA = k.nameLocalName, valueA = v }
                  ) (Mp.toList anElement.elementAttributes)
        in
        case anElement.elementName of
          "label" -> Right $ Tm.LabelFE attributes
          "field" -> Right $ Tm.FieldFE attributes
          "image" -> Right $ Tm.ImageFE attributes
          "separator" -> Right $ Tm.SeparatorFE attributes
          "newline" -> Right $ Tm.NewlineFE attributes
          "button" -> Right $ Tm.ButtonFE attributes
          "link" -> Right $ Tm.LinkFE attributes
          "child" -> Right $ Tm.ChildFE attributes
          _ ->
            if anElement.elementName `elem` ["notebook", "page", "group", "hpaned", "vpaned", "child"] then
              let
                subNodes = concatMap (anyElement . fromNode) anElement.elementNodes
                children = map parseFormElement subNodes
              in
              if null (Ei.lefts children) then
                case anElement.elementName of
                  "notebook" -> Right $ Tm.NotebookFE attributes (Ei.rights children)
                  "page" -> Right $ Tm.PageFE attributes (Ei.rights children)
                  "group" -> Right $ Tm.GroupFE attributes (Ei.rights children)
                  "hpaned" -> Right $ Tm.HPanedFE attributes (Ei.rights children)
                  "vpaned" -> Right $ Tm.VPanedFE attributes (Ei.rights children)
                  _ -> Left $ "@[parseFormElement] internal err, unknown element in filtered situation: " <> T.unpack anElement.elementName.nameLocalName
              else
                Left $ "@[parseFormElement] errs: " <> L.intercalate "; " (Ei.lefts children)
            else
              Left $ "@[parseFormElement] invalid element: " <> T.unpack anElement.elementName.nameLocalName
      _ -> Left $ "@[parseFormElement] not a NodeElement:" <> show (node aCursor)


parseTmpFunc :: Cursor -> Either String Tm.Definition
parseTmpFunc c = Left "@[parseTmpFunc] not implemented"


parseField :: Cursor -> Tm.Field
parseField !c =
  case attribute "eval" c of
    [v] -> Tm.Field {kindF = Tm.EvalFK, valueF = v}
    _ -> case attribute "ref" c of
          [v] -> Tm.Field {kindF = Tm.ReferenceFK, valueF = v}
          _   -> Tm.Field {kindF = Tm.LabelFK, valueF = getContent c}


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

