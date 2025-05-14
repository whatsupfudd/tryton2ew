{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use second" #-}
module Commands.MenuFinder where

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

import qualified Options.Runtime as Rto
import qualified Parsing.Python as Py
import qualified Generation.Sql as Sq


data MenuItem = MenuItem {
    idMI       :: !T.Text
  , parentMI   :: !(Maybe T.Text)
  , nameMI     :: !(Maybe T.Text)
  , iconMI     :: !(Maybe T.Text)
  , childrenMI :: ![MenuItem]
  }
  deriving (Show, Eq)


data Definition = Definition { 
    defId     :: !T.Text
  , defModel  :: !T.Text
  , defFields :: !(Mp.Map T.Text Field)
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

data TargetFileKind =
    XmlFile
  | PotFile
  | PyFile
  deriving (Show, Eq)


data TargetFile = TargetFile {
    kindTF :: !TargetFileKind
  , pathTF :: !FilePath
  }


menuFinderCmd :: FilePath -> Rto.RunOptions -> IO ()
menuFinderCmd filePath rtOpts = do
  targetFiles <- getTargetFiles filePath
  let
    !xmlFiles = [ tf.pathTF | tf <- targetFiles, tf.kindTF == XmlFile ]
    !potFiles = [ tf.pathTF | tf <- targetFiles, tf.kindTF == PotFile ]
    !pyFiles  = [ tf.pathTF | tf <- targetFiles, tf.kindTF == PyFile ]
  putStrLn $ "nbr xmlFiles: " <> show (length xmlFiles)
  handleXmlFiles xmlFiles
  putStrLn $ "nbr potFiles: " <> show (length potFiles)
  -- handlePotFiles potFiles
  handlePyFiles pyFiles



getTargetFiles :: FilePath -> IO [TargetFile]
getTargetFiles !dir = do
    !entries <- listDirectory dir
    !paths <- forM entries $ \name -> do
      let path = dir </> name
      !isDir <- doesDirectoryExist path
      if isDir then
        getTargetFiles path
      else
        case takeExtension path of
          ".xml" -> pure [TargetFile { kindTF = XmlFile, pathTF = path }]
          ".pot" -> pure [TargetFile { kindTF = PotFile, pathTF = path }]
          ".py"  -> pure [TargetFile { kindTF = PyFile, pathTF = path }]
          _       -> pure []
    pure (concat paths)


handleXmlFiles :: [FilePath] -> IO ()
handleXmlFiles !files = do
  !mbItems <- forM files $ \aFile -> do
    !startItemsTime <- getCurrentTime
    !xmlDoc <- loadXmlFile aFile
    !endItemsTime <- getCurrentTime
    case xmlDoc of
      Just aDoc ->
        let
          !items = extractItems aDoc
          !defs = extractDefinitions aDoc
        in
        pure (items, defs, diffUTCTime endItemsTime startItemsTime)
      Nothing -> pure ([], [], 0)
  putStrLn $ "time to load xml Files: " <> show (sum (map (\(_, _, t) -> t) mbItems))

  let
    !tree = buildTree $ concatMap (\(items, _, _) -> items) mbItems
  printTree tree 0
  putStrLn "\n--------------------------------\n"

  !startDefsTime <- getCurrentTime
  let
    !allDefs = concatMap (\(_, defs, _) -> defs) mbItems
    !modelDefs =
      let
        !initMap = Mp.fromList [(l.defModel, []) | l <- allDefs]
      in
      foldl (\accum d -> Mp.insertWith (++) d.defModel [d] accum) initMap allDefs
  !endDefsTime <- getCurrentTime
  printDefinitions modelDefs
  pure ()


handlePotFiles :: [FilePath] -> IO ()
handlePotFiles files = pure ()


handlePyFiles :: [FilePath] -> IO ()
handlePyFiles files = do
  putStrLn "--------------------------------"
  putStrLn $ "nbr pyFiles:  " <> show (length files)

  !allElements <- forM files $ \aFile -> do
    !elements <- Py.extractElements aFile
    pure (aFile, elements)

  putStrLn "\n-- Debug elements: --\n"
  mapM_ (\(f, ms) -> putStrLn $ f <> ":\n" <> show ms <> "\n") allElements
  putStrLn "\n--------------------------------\n"
  let
    eiFileDefs = map (\(f, elements) -> (f, Sq.genTableDef elements)) allElements
  mapM_ (\(f, eiTableDefs) ->
      let
        msgs = foldl(\accum td ->
           case td of
             Left err -> accum <> err <> "\n"
             Right tableDef -> accum <> show tableDef <> "\n"
          ) "" eiTableDefs
      in
      putStrLn $ "@[handlePyFiles] " <> f <> if msgs == "" then "" else ":\n" <> msgs
    ) eiFileDefs


loadXmlFile :: FilePath -> IO (Maybe Document)
loadXmlFile filePath = do
  result <- try (X.readFile X.def filePath) :: IO (Either SomeException Document)
  case result of
      Right doc -> pure $ Just doc
      Left _    -> pure Nothing


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
      MenuItem mid parent nameVal iconAttr []


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
      defId = rid
      , defModel = model
      , defFields = fields
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


printTree :: [MenuItem] -> Int -> IO ()
printTree !items !depth =
  let
    !offset = T.replicate (depth * 2) " "
  in
  forM_ items $ \i ->
      let
        !label = maybe ("id=" <> i.idMI) ("menu: " <>) i.nameMI
        !parent = ("p=" <>) <$> i.parentMI
        !icon = ("i=" <>) <$> i.iconMI
        !piPart = T.intercalate ", " (maybeToList parent <> maybeToList icon)
        !details = " [id=" <> i.idMI <> if piPart == "" then "" else (", " <> piPart) <> "]"
      in do
      case depth of
        0 -> putStrLn . T.unpack $ label <> details
        _ -> putStrLn . T.unpack $ offset <> label <> maybe "" (\i -> " [" <> i <> "]") icon
      printTree i.childrenMI (depth + 1)


printDefinitions :: Mp.Map T.Text [Definition] -> IO ()
printDefinitions !modelDefs =
  forM_ (Mp.toList modelDefs) $ \(modelName, defs) -> do
    putStrLn . T.unpack $ "-- model: " <> modelName <> " --"
    forM_ defs $ \d -> do
        putStrLn . T.unpack $ d.defId <> ":"
        forM_ (Mp.toList (defFields d)) $ \(k,v) ->
          putStrLn . T.unpack $ "  - " <> k <> ": " <> showField v

showField :: Field -> T.Text
showField !f = case f.kindF of
  LabelFK -> f.valueF
  EvalFK -> "#" <> f.valueF
  ReferenceFK -> "@" <> f.valueF
