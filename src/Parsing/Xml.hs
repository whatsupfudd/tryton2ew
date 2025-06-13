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


data MenuItem = MenuItem {
    idMI :: T.Text
  , parentMI :: Maybe T.Text
  , nameMI :: Maybe T.Text
  , iconMI :: Maybe T.Text
  , seqOrdMI :: Int
  , childrenMI :: [MenuItem]
  , actionMI :: Maybe T.Text
  }
  deriving (Show)


data ViewDefs = ViewDefs {
    trees :: Mp.Map T.Text ([Attribute], [TreeElement])
  , forms :: Mp.Map T.Text ([Attribute], [FormElement])
  , lists :: Mp.Map T.Text ([Attribute], [ToDoElement])
  , graphs :: Mp.Map T.Text ([Attribute], [ToDoElement])
  , boards :: Mp.Map T.Text ([Attribute], [ToDoElement])
  , calendars :: Mp.Map T.Text ([Attribute], [ToDoElement])
  , errors :: Mp.Map T.Text [String]
  }
  deriving (Show)


data Definition =
  ModelDF ClassInstance
  | TreeDF [Attribute] [TreeElement]
  | FormDF [Attribute] [FormElement]
  | ListFormDF [Attribute] [ToDoElement]
  | GraphDF [Attribute] [ToDoElement]
  | BoardDF [Attribute] [ToDoElement]
  | CalendarDF [Attribute] [ToDoElement]
  | ParseErrorDF String
  deriving (Show)


data ClassInstance = ClassInstance {
    idDF     :: !T.Text
  , modelDF  :: !T.Text
  , fieldsDF :: !(Mp.Map T.Text Field)
  }
  deriving (Show)


data TreeElement =
  FieldTE [Attribute] [NFixElement]
  | ButtonTE [Attribute]
  deriving (Show)

data NFixElement =
  PrefixNF [Attribute]
  | SuffixNF [Attribute]
  deriving (Show)


data FormElement =
  LabelFE [Attribute]
  | FieldFE [Attribute]
  | ImageFE [Attribute]
  | SeparatorFE [Attribute]
  | NewlineFE [Attribute]
  | ButtonFE [Attribute]
  | LinkFE [Attribute]
  | NotebookFE [Attribute] [FormElement]
  | PageFE [Attribute] [FormElement]
  | GroupFE [Attribute] [FormElement]
  | HPanedFE [Attribute] [FormElement]
  | VPanedFE [Attribute] [FormElement]
  | ChildFE [Attribute]
  deriving (Show)

data Attribute = Attribute {
      nameA :: T.Text
    , valueA :: T.Text
  }
  deriving (Show)

-- TODO: implement all the elements for ListForm, Graph, Board and Calendar.
data ToDoElement = ToDoElement { nameTDE :: T.Text, childrenTDE :: [ToDoElement] }
  deriving (Show)


data FieldKind =
  LabelFK
  | EvalFK
  | ReferenceFK
  deriving (Show)


data Field = Field {
    kindF :: !FieldKind
  , valueF :: !T.Text
  }
  deriving (Show)


extractMenuItems :: Document -> [MenuItem]
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
        parent = listToMaybe $ attribute "parent" c
        nameAttr = listToMaybe (attribute "name" c)
        labelTxt = listToMaybe (c $/ element "label" &/ content)
        iconAttr = listToMaybe (attribute "icon" c)
        sequence = maybe 10000000 (read . T.unpack) (listToMaybe (attribute "sequence" c))
        action = listToMaybe (attribute "action" c)
        nameVal = nameAttr <|> labelTxt
      in
      MenuItem {
        idMI = mid, parentMI = parent, nameMI = nameVal, iconMI = iconAttr, seqOrdMI = sequence, childrenMI = [], actionMI = action
      }


processDefinitions :: [(T.Text, [Definition])] -> (Mp.Map T.Text [ClassInstance], ViewDefs)
processDefinitions allDefs =
  let
    (modelDefs, trees, forms, lists, graphs, boards, calendars, errors) =
      foldl (\(modelA, treeA, formA, listA, graphA, boardA, calendarA, errorA) (fileName, someDefs) ->
        foldl (\(modelB, treeB, formB, listB, graphB, boardB, calendarB, errorB) aDef -> case aDef of
          ModelDF modelEle -> (Mp.insertWith (<>) modelEle.modelDF [modelEle] modelB, treeB, formB, listB, graphB, boardB, calendarB, errorB)
          TreeDF attrs elements -> (modelB, Mp.insert fileName (attrs, elements) treeB, formB, listB, graphB, boardB, calendarB, errorB)
          FormDF attrs elements -> (modelB, treeB, Mp.insert fileName (attrs, elements) formB, listB, graphB, boardB, calendarB, errorB)
          ListFormDF attrs elements -> (modelB, treeB, formB, Mp.insert fileName (attrs, elements) listB, graphB, boardB, calendarB, errorB)
          GraphDF attrs elements -> (modelB, treeB, formB, listB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) graphB, boardB, calendarB, errorB)
          BoardDF attrs elements -> (modelB, treeB, formB, listB, graphB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) boardB, calendarB, errorB)
          CalendarDF attrs elements -> (modelB, treeB, formB, listB, graphB, boardB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) calendarB, errorB)
          ParseErrorDF errs -> (modelB, treeB, formB, listB, graphB, boardB, calendarB, Mp.insertWith (<>) fileName [errs] errorB)
        ) (modelA, treeA, formA, listA, graphA, boardA, calendarA, errorA) someDefs
      ) (Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty) allDefs
  in
  (modelDefs, ViewDefs trees forms lists graphs boards calendars errors)


extractDefinitions :: FilePath -> Document -> ([Definition], (String, [String], [T.Text]))
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
              [ ParseErrorDF err ] ->
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
  where
  parseRec c =
    let
      !rid = singleAttr "id" c
      !model = singleAttr "model" c
      !fields = Mp.fromList [
                  (singleAttr "name" field, parseField field) | field <- c $/ element "field"
                ]
    in
    ModelDF $ ClassInstance {
      idDF = rid
      , modelDF = model
      , fieldsDF = fields
    }
  singleAttr :: X.Name -> Cursor -> T.Text
  singleAttr !n !c = case attribute n c of
                      [v] -> v
                      _   -> error $ "Expected single '" ++ T.unpack n.nameLocalName ++ "' attribute"

  parseTargetElement :: Document -> X.Name -> (Cursor -> Either String Definition) -> [Definition]
  parseTargetElement aDoc targetName parseFunc =
    let
      cursor = fromDocument aDoc
      targetNodes = cursor $| element targetName
      eiParseRez = map parseFunc targetNodes
    in
    if null (Ei.lefts eiParseRez) then
      if null (Ei.rights eiParseRez) then
        [ ParseErrorDF $ "@[parseTargetElement] no " <> T.unpack targetName.nameLocalName <> " found, " <> show targetNodes
         , ParseErrorDF $ "@[parseTargetElement] top element: " <> show (case node cursor of NodeElement anElement -> anElement.elementName.nameLocalName; _ -> "unknown")
        ]
      else
        Ei.rights eiParseRez
    else
      [ ParseErrorDF err | err <- Ei.lefts eiParseRez ]


parseTree :: Cursor -> Either String Definition
parseTree c =
  case node c of
    NodeElement anElement ->
      case anElement.elementName of
        "tree" ->
          let
            subNodes = concatMap (anyElement . fromNode) anElement.elementNodes
            attributes = map (\(k,v) ->
                      Attribute { nameA = k.nameLocalName, valueA = v }
                  ) (Mp.toList anElement.elementAttributes)
            children = map parseTreeElement subNodes
          in
          if null (Ei.lefts children) then
            Right $ TreeDF attributes (Ei.rights children)
          else
            Left $ "@[parseTree] errs: " <> L.intercalate "; " (Ei.lefts children)
        anotherName -> Left $ "@[parseTree] invalid element: " <> T.unpack anotherName.nameLocalName
    anotherNode -> Left $ "@[parseTree] didn't get an element." <> show anotherNode
  where
  parseTreeElement :: Cursor -> Either String TreeElement
  parseTreeElement aCursor =
    case node aCursor of
      NodeElement anElement ->
        let
          attributes = map (\(k,v) ->
                      Attribute { nameA = k.nameLocalName, valueA = v }
                  ) (Mp.toList anElement.elementAttributes)
        in
        case anElement.elementName of
          "field" ->
            let
              subFixes = concatMap (anyElement . fromNode) anElement.elementNodes
              nfixes = map parseNFixElement subFixes
            in
            if null (Ei.lefts nfixes) then
              Right $ FieldTE attributes (Ei.rights nfixes)
            else
              Left $ "@[parseTreeElement] errs: " <> L.intercalate "; " (Ei.lefts nfixes)
          "button" -> Right $ ButtonTE attributes
          _ -> Left $ "@[parseTreeElement] invalid element: " <> T.unpack anElement.elementName.nameLocalName
      _ -> Left $ "@[parseTreeElement] not a NodeElement:" <> show (node aCursor)
  parseNFixElement :: Cursor -> Either String NFixElement
  parseNFixElement aCursor =
    case node aCursor of
      NodeElement anElement ->
        let
          attributes = map (\(k,v) ->
                      Attribute { nameA = k.nameLocalName, valueA = v }
                  ) (Mp.toList anElement.elementAttributes)
        in
        case anElement.elementName of
          "prefix" -> Right $ PrefixNF attributes
          "suffix" -> Right $ SuffixNF attributes
          _ -> Left $ "@[parseNFixElement] invalid element: " <> T.unpack anElement.elementName.nameLocalName
      _ -> Left $ "@[parseNFixElement] not a NodeElement:" <> show (node aCursor)


parseForm :: Cursor -> Either String Definition
parseForm aCursor =
  case node aCursor of
    NodeElement anElement ->
      case anElement.elementName of
        "form" ->
          let
            subNodes = concatMap (anyElement . fromNode) anElement.elementNodes
            attributes = map (\(k,v) ->
                      Attribute { nameA = k.nameLocalName, valueA = v }
                  ) (Mp.toList anElement.elementAttributes)
            children = map parseFormElement subNodes
          in
          if null (Ei.lefts children) then
            Right $ FormDF attributes (Ei.rights children)
          else
            Left $ "@[parseForm] errs: " <> L.intercalate "; " (Ei.lefts children)
        "data" -> Left "@[parseForm] unsupported <data> in _form file."
        anotherName -> Left $ "@[parseForm] invalid element: " <> T.unpack anotherName.nameLocalName
    anotherNode -> Left $ "@[parseForm] didn't get an element." <> show anotherNode
  where
  parseFormElement :: Cursor -> Either String FormElement
  parseFormElement aCursor =
    case node aCursor of
      NodeElement anElement ->
        let
          attributes = map (\(k,v) ->
                      Attribute { nameA = k.nameLocalName, valueA = v }
                  ) (Mp.toList anElement.elementAttributes)
        in
        case anElement.elementName of
          "label" -> Right $ LabelFE attributes
          "field" -> Right $ FieldFE attributes
          "image" -> Right $ ImageFE attributes
          "separator" -> Right $ SeparatorFE attributes
          "newline" -> Right $ NewlineFE attributes
          "button" -> Right $ ButtonFE attributes
          "link" -> Right $ LinkFE attributes
          "child" -> Right $ ChildFE attributes
          _ ->
            if anElement.elementName `elem` ["notebook", "page", "group", "hpaned", "vpaned", "child"] then
              let
                subNodes = concatMap (anyElement . fromNode) anElement.elementNodes
                children = map parseFormElement subNodes
              in
              if null (Ei.lefts children) then
                case anElement.elementName of
                  "notebook" -> Right $ NotebookFE attributes (Ei.rights children)
                  "page" -> Right $ PageFE attributes (Ei.rights children)
                  "group" -> Right $ GroupFE attributes (Ei.rights children)
                  "hpaned" -> Right $ HPanedFE attributes (Ei.rights children)
                  "vpaned" -> Right $ VPanedFE attributes (Ei.rights children)
                  _ -> Left $ "@[parseFormElement] internal err, unknown element in filtered situation: " <> T.unpack anElement.elementName.nameLocalName
              else
                Left $ "@[parseFormElement] errs: " <> L.intercalate "; " (Ei.lefts children)
            else
              Left $ "@[parseFormElement] invalid element: " <> T.unpack anElement.elementName.nameLocalName
      _ -> Left $ "@[parseFormElement] not a NodeElement:" <> show (node aCursor)


parseTmpFunc :: Cursor -> Either String Definition
parseTmpFunc c = Left "@[parseTmpFunc] not implemented"


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


buildMenuTree :: [MenuItem] -> [MenuItem]
buildMenuTree !items =
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


printMenuTree :: [MenuItem] -> Int -> FilePath -> IO ()
printMenuTree !items !depth !destPath =
  TIO.writeFile (destPath </> "tree.txt") . T.intercalate "\n" $
    printMenuTree' items depth
  where
  printMenuTree' :: [MenuItem] -> Int -> [T.Text]
  printMenuTree' !items !depth =
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
      prefix <> "\n" <> T.intercalate "\n" (printMenuTree' i.childrenMI (depth + 1))
    ) items


printClassInstances :: Mp.Map T.Text [ClassInstance] -> FilePath -> IO ()
printClassInstances defMap destPath =
  TIO.writeFile (destPath </> "classInstances.txt") . T.intercalate "\n" $
    map (\(pathName, defs) ->
        "-- class: " <> pathName <> " --\n"
        <> T.intercalate "\n" (
          map (\modelEle ->
                  modelEle.idDF <> ":\n"
                  <> T.intercalate "\n" (map (\(k,v) ->
                        "  - " <> k <> ": " <> showField v
                      ) (Mp.toList modelEle.fieldsDF))
          ) defs
        )
      ) (Mp.toList defMap)

printViewDefs :: ViewDefs -> FilePath -> IO ()
printViewDefs viewDefs destPath =
  let
    treeOut = map (\(fileName, (attrs, elements)) -> "-- tree: " <> fileName <> " --\nattrs: "
                  <> T.intercalate "\n  " (map (T.pack . show) attrs)
                  <> "\nelements: " <> T.intercalate "\n  " (map (T.pack . show) elements)
      ) (Mp.toList viewDefs.trees)
    formOut = map (\(fileName, (attrs, elements)) -> "-- form: " <> fileName <> " --\nattrs: "
                  <> T.intercalate "\n  " (map (T.pack . show) attrs)
                  <> "\nelements: " <> T.intercalate "\n  " (map (T.pack . show) elements)
              ) (Mp.toList viewDefs.forms)
    listOut = map (\(fileName, (attrs, elements)) -> "-- list: " <> fileName <> " --\nattrs: "
                  <> T.intercalate "\n  " (map (T.pack . show) attrs)
                  <> "\nelements: " <> T.intercalate "\n  " (map (T.pack . show) elements)
              ) (Mp.toList viewDefs.lists)
    graphOut = map (\(fileName, (attrs, elements)) -> "-- graph: " <> fileName <> " --\nattrs: "
                  <> T.intercalate "\n  " (map (T.pack . show) attrs)
                  <> "\nelements: " <> T.intercalate "\n  " (map (T.pack . show) elements)
              ) (Mp.toList viewDefs.graphs)
    boardOut = map (\(fileName, (attrs, elements)) -> "-- board: " <> fileName <> " --\nattrs: "
                  <> T.intercalate "\n  " (map (T.pack . show) attrs)
                  <> "\nelements: " <> T.intercalate "\n  " (map (T.pack . show) elements)
              ) (Mp.toList viewDefs.boards)
    calendarOut = map (\(fileName, (attrs, elements)) -> "-- calendar: " <> fileName <> " --\nattrs: "
                  <> T.intercalate "\n  " (map (T.pack . show) attrs)
                  <> "\nelements: " <> T.intercalate "\n  " (map (T.pack . show) elements)
              ) (Mp.toList viewDefs.calendars)
    errorOut = map (\(fileName, errors) -> "-- error: " <> fileName <> " --\n"
                  <> T.intercalate "\n" (map T.pack errors)
              ) (Mp.toList viewDefs.errors)
  in
  TIO.writeFile (destPath </> "viewDefs.txt") . T.intercalate "\n" $
    treeOut <> formOut <> listOut <> graphOut <> boardOut <> calendarOut <> errorOut

printViewErrs :: ViewDefs -> FilePath -> IO ()
printViewErrs viewDefs destPath =
  let
    errorOut = map (\(fileName, errors) -> "-- error: " <> fileName <> " --\n"
                  <> T.intercalate "\n" (map T.pack errors)
              ) (Mp.toList viewDefs.errors)
  in
  TIO.writeFile (destPath </> "viewDefs.txt") $ T.intercalate "\n" errorOut


showField :: Field -> T.Text
showField !f = case f.kindF of
  LabelFK -> f.valueF
  EvalFK -> "#<" <> f.valueF <> ">"
  ReferenceFK -> "@<" <> f.valueF <> ">"
