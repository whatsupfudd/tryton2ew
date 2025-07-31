{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}

module Tryton.Load where

import Control.Monad (forM)
import Control.Exception (try, SomeException)

import qualified Data.ByteString as Bs
import Data.Either (lefts, rights, partitionEithers)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map.Strict as Mp
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>), splitDirectories, splitFileName, takeBaseName)

import qualified Data.ConfigFile as Cf

import Text.XML (Document(..), Element(..), Node(..), Prologue(..), renderText)
import qualified Text.XML as X
import Text.XML.Cursor (Cursor, attribute, child, content
                        , element, fromDocument, node, ($//), ($/), (>=>), (&/))

import Options.Cli (ImporterOptions(..))
import qualified Parsing.Python as Py
import qualified Parsing.Xml as Xm
import qualified Parsing.Pot as Po
import qualified Generation.Sql as Sq

import qualified Tryton.Process as Tp
import Tryton.Types

{-
importOpts control:
  , schemaIO :: Bool
  , dataPrepIO :: Bool
  , noAppIO :: Bool
  , noLocalesIO :: Bool
-}

loadConfiguration :: ImporterOptions -> FilePath -> [TargetFile] -> IO (Either String [ModuleSrcTT])
loadConfiguration importOpts rootPath targetFiles =
  let
    !configFiles = [ tf.pathTF | tf <- targetFiles, tf.kindTF == CfgFile ]
    !xmlFiles = [ T.pack tf.pathTF | tf <- targetFiles, tf.kindTF == XmlFile ]
    !potFiles = [ T.pack tf.pathTF | tf <- targetFiles, tf.kindTF == PotFile ]
    !pyFiles = [ T.pack tf.pathTF | tf <- targetFiles, tf.kindTF == PyFile ]
  in do
  configRez <- forM configFiles $ \aCfgFile ->
    -- putStrLn $ "@[importerCmd] config file: " <> aFile
    case drop 1 . take 2 . reverse . splitDirectories $ aCfgFile of
      [] -> pure $ Left "@[importerCmd] cfg readfile err: no module name"
      (moduleName : _) -> do
        eiConfig <- Cf.readfile Cf.emptyCP aCfgFile
        case eiConfig of
          Left err -> pure . Left $ "@[importerCmd] cfg readfile err: " <> show err
          Right config ->
            case Mp.lookup "tryton" config.content of
              Nothing -> pure $ Left "@[importerCmd] cfg readfile err: no tryton section"
              Just configMap ->
                let
                  depends = maybe [] (filter (/= "") . lines) (Mp.lookup "depends" configMap)
                  (target, dataXml, otherXml) = case Mp.lookup "xml" configMap of
                    Nothing -> (Nothing, [], [])
                    Just xmlStr ->
                      let
                        xmlFiles = filter (/= "") . lines $ xmlStr
                      in
                      case xmlFiles of
                        [] -> (Nothing, [], [])
                        _ -> foldr (\aPath (mbTarget, dataAccum, otherAccum) ->
                            case length $ splitDirectories aPath of
                              1 ->
                                if takeBaseName aPath == moduleName <> "_view" then
                                  (Just aPath, dataAccum, otherAccum)
                                else
                                  (mbTarget, dataAccum, aPath : otherAccum)
                              _ -> if "data/" `T.isPrefixOf` T.pack aPath then
                                (mbTarget, drop 5 aPath : dataAccum, otherAccum)
                              else
                                (mbTarget, dataAccum, aPath : otherAccum)
                          ) (Nothing, [], []) xmlFiles
                in do
                pure . Right $ ModuleSrcTT {
                  nameMT = moduleName
                  , locationMT = T.pack . fst $ splitFileName aCfgFile
                  , dependsMT = depends
                  , targetMT = target
                  , dataSpecMT = dataXml
                  , supportSpecMT = otherXml
                  , localesMT = []
                  , viewsMT = []
                  , miscXmlMT = []
                  , logicMT = []
                }
  case lefts configRez of
    [] ->
      let
        ((leftXml, leftPy, leftLocales), consolidated) = foldl (\((xmlLO, pyLO, localesLO), modules) aModule ->
            let
              locationLength = T.length aModule.locationMT
              (updXmlF, inXmlF) = foldl (\(accum, inAccum) aPath ->
                  if aModule.locationMT `T.isPrefixOf` aPath then
                    (accum, T.drop locationLength aPath : inAccum)
                  else
                    (aPath : accum, inAccum)
                  ) ([], []) xmlLO
              (updPyF, inPyF) = foldl (\(accum, inAccum) aPath ->
                  if aModule.locationMT `T.isPrefixOf` aPath then
                    (accum, T.drop locationLength aPath : inAccum)
                  else
                    (aPath : accum, inAccum)
                  ) ([], []) pyLO
              (updLocales, inLocales) = foldl (\(accum, inAccum) aPath ->
                  if aModule.locationMT `T.isPrefixOf` aPath then
                    (accum, T.drop locationLength aPath : inAccum)
                  else
                    (aPath : accum, inAccum)
                  ) ([], []) localesLO
              (views, otherXml) = foldl (\(views, otherXml) aPath ->
                  if "view/" `T.isPrefixOf` aPath then
                    (T.drop 5 aPath : views, otherXml)
                  else
                    (views, aPath : otherXml)
                  ) ([], []) inXmlF
              updModule = aModule {
                viewsMT = map T.unpack views
                , miscXmlMT = map T.unpack otherXml
                , logicMT = map T.unpack inPyF
                , localesMT = map T.unpack inLocales
              }
            in
            ((updXmlF, updPyF, updLocales), updModule : modules)
          ) ((xmlFiles, pyFiles, potFiles), []) (rights configRez)
      in do
      putStrLn $ "@[importerCmd] leftXml: " <> show leftXml
      putStrLn $ "@[importerCmd] leftPy: " <> show leftPy
      putStrLn $ "@[importerCmd] leftLocales: " <> show leftLocales
      pure $ Right consolidated
    errs -> pure $ Left $ L.intercalate "\n" errs

loadModule :: ImporterOptions -> ModuleSrcTT -> IO (Either String FullModuleTT)
loadModule importOpts aModule = do
  case aModule.targetMT of
    Nothing ->
      pure $ Right $ FullModuleTT {
        srcModuleFM = aModule
        , menusFM = []
        , actWinsFM = []
        , xmlDefsFM = []
        , localesFM = Mp.empty
        , viewDefsFM = Nothing
        , logicFM = Mp.empty
        , sqlDefsFM = Mp.empty
      }
    Just targetFile ->
      let
        basePath = T.unpack aModule.locationMT
      in do
      -- Start with the main definitions:
      xmlDoc <- loadXmlFile (basePath </> targetFile)
      case xmlDoc of
        Nothing -> pure . Left $ "@[loadModule] no xml file for module: " <> aModule.nameMT
        Just aDoc ->
          let
            items = Xm.extractMenuItems aDoc
            moduleViews = Xm.parseModuleView aDoc
          in do
          viewsRez <- forM aModule.viewsMT $ \xmlFilePath -> do
            aRez <- loadXmlFile (basePath </> "view" </> xmlFilePath)
            case aRez of
              Nothing -> pure . Left $ "@[loadModule] loadXmlFile empty on: " <> aModule.nameMT <> "." <> xmlFilePath
              Just aDoc ->
                let
                  baseFileName = takeBaseName xmlFilePath
                  (views, _) = Xm.parseUiView baseFileName aDoc
                in do
                pure $ Right (T.pack baseFileName, views)
          (_, viewDefs) <- case lefts viewsRez of
            [] -> pure . Tp.processDefinitions $ rights viewsRez
            errs -> do
              putStrLn $ "@[loadModule] error loading views: " <> L.intercalate "\n" errs
              pure . Tp.processDefinitions $ rights viewsRez
          {- PotFiles will be handled in a separate step. -}
          potFiles <- if importOpts.noLocalesIO then
            pure Mp.empty
          else
            handlePotFiles [basePath </> aLocale | aLocale <- aModule.localesMT] importOpts.destPathIO
          --}
          logicElements <-
            handlePyFiles [basePath </> aPyFile | aPyFile <- aModule.logicMT] importOpts.destPathIO
          let
            eiClassDefs = map (\(f, elements) -> (f, Sq.genTableDefs elements)) logicElements
          tableDefs <- mapM (\(fileName, eiList) ->
                case lefts eiList of
                  [] -> pure (fileName, rights eiList)
                  errs -> do
                    putStrLn $ "@[loadModule] genTableDefs errs: " <> L.intercalate "\n" errs
                    pure (fileName, rights eiList)
              ) eiClassDefs
          pure $ Right $ FullModuleTT {
            srcModuleFM = aModule
            , menusFM = items
            , actWinsFM = concatMap (\case
                     ModelDF clInstance -> [clInstance]
                     _ -> []
                  ) moduleViews
            , xmlDefsFM = []
            , localesFM = Mp.empty
            , viewDefsFM = Just viewDefs
            , logicFM = Mp.fromList [(T.pack aPyFile, elements) | (aPyFile, elements) <- logicElements]
            , sqlDefsFM = Mp.fromList $ concat [[(T.decodeUtf8 sqlTable.nameST, sqlTable) | sqlTable <- sqlTables ] | (filePath, sqlTables) <- tableDefs]
          }

consolidateModules :: [FullModuleTT] -> Either String TrytonApp
consolidateModules modules =
  let
    !tree = Tp.buildMenuTree $ concatMap menusFM modules
  in
  Right $ TrytonApp {
    modulesTA = modules
    , unifiedMenuTA = tree
    , localesTA = Mp.empty
  }
  {-
  -}


handleXmlFiles :: [FilePath] -> FilePath -> IO XmlDefs
handleXmlFiles !files destPath = do
  !startItemsTime <- getCurrentTime
  !mbItems <- forM files $ \aFile -> do
    !xmlDoc <- loadXmlFile aFile
    case xmlDoc of
      Just aDoc ->
        let
          items = Xm.extractMenuItems aDoc
          (views, (fileName, dirPath, nameParts)) = Xm.extractDefinitions aFile aDoc
        in do
        -- putStrLn $ "@[handleXmlFiles] fn: " <> fileName <> ", dir: " <> show dirPath <> ", nameParts: " <> show nameParts <> "\n   defs: " <> show views
        pure (items, (T.pack fileName, views))
      Nothing -> pure ([], ("", []))
  !endItemsTime <- getCurrentTime
  -- putStrLn $ "@[handleXmlFiles] time to load xml files: " <> show (diffUTCTime endItemsTime startItemsTime)

  let
    !tree = Tp.buildMenuTree $ concatMap fst mbItems
  Tp.printMenuTree tree 0 destPath
  -- putStrLn "\n--------------------------------\n"

  !startDefsTime <- getCurrentTime
  let
    (classInstances, viewDefs) = Tp.processDefinitions $ map snd mbItems
  -- putStrLn $ "@[handleXmlFiles] modelDefs: " <> show modelDefs
  -- putStrLn $ "@[handleXmlFiles] viewDefs: " <> show viewDefs
  !endDefsTime <- getCurrentTime
  --putStrLn $ "@[handleXmlFiles] time to make viewDefs: " <> show (diffUTCTime endDefsTime startDefsTime)
  Tp.printClassInstances classInstances destPath
  Tp.printViewErrs viewDefs destPath
  pure (tree, classInstances, viewDefs)


loadXmlFile :: FilePath -> IO (Maybe Document)
loadXmlFile filePath = do
  result <- try (X.readFile X.def filePath) :: IO (Either SomeException Document)
  case result of
      Right doc -> pure $ Just doc
      Left _    -> pure Nothing


handlePotFiles :: [FilePath] -> FilePath -> IO Po.LocaleDefs
handlePotFiles files destPath = do
  !startParseTime <- getCurrentTime
  !potFiles <- forM files $ \aFile -> do
    !potDoc <- Po.parseLocFileWithDiagnostics aFile
    let
      moduleName = takeDirectory aFile
      fileName = takeFileName aFile
    pure (T.pack moduleName, T.pack fileName, potDoc)
  !endParseTime <- getCurrentTime
  -- putStrLn $ "@[handlePotFiles] time to parse: " <> show (map takeBaseName files) <> ": "<> show (diffUTCTime endParseTime startParseTime)

  -- putStrLn "\n-- Debug potFiles: --\n"
  {- This will be handled on a per-module basis:
  TIO.writeFile (destPath </> "potFiles.txt") . T.pack . L.intercalate "\n" $ map (\(mName, fileName, rez) ->
      T.unpack (mName <> ":" <> fileName) <> "\n"
      <> case rez of
            Left err -> "err: " <> show err
            Right potDoc -> show potDoc
    ) potFiles
  -}
  -- putStrLn "\n--------------------------------\n"

  !startProcTime <- getCurrentTime
  let
    (errors, locEntries) = foldr (\(mName, locale, aResult) (errs, corrects) ->
        case aResult of
          Left err -> (err : errs, corrects)
          Right locEntries -> (errs, (T.encodeUtf8 mName, T.encodeUtf8 locale, locEntries) : corrects)
      ) ([], []) potFiles
    reorgLocales =
      foldl (\accum (mName, locale, locFile) ->
        let
          updLocale = Po.fixLocale locale mName
        in
        case Mp.lookup updLocale accum of
            Just moduleMap ->
              let
                updModuleMap = Mp.insertWith (++) mName locFile.entriesFI moduleMap
              in
              Mp.insert updLocale updModuleMap accum
            Nothing ->
              let
                newModuleMap = Mp.singleton mName locFile.entriesFI
              in
              Mp.insert updLocale newModuleMap accum
        ) (Mp.empty :: Mp.Map Bs.ByteString (Mp.Map Bs.ByteString [Po.LocEntry])) locEntries
  !endProcTime <- getCurrentTime
  -- putStrLn $ "@[handlePotFiles] time to process: " <> show (diffUTCTime endProcTime startProcTime)
  -- TIO.writeFile (destPath </> "reorgLocales.txt") $ T.pack (show reorgLocales)
  pure reorgLocales


handlePyFiles :: [FilePath] -> FilePath -> IO [(FilePath, [Py.LogicElement])]
handlePyFiles files destPath = do
  !startParseTime <- getCurrentTime
  !allElements <- forM files $ \aFile -> do
    !elements <- Py.extractElements aFile
    pure (aFile, elements)
  !endParseTime <- getCurrentTime
  -- putStrLn $ "@[handlePyFiles] time to parse: " <> show (diffUTCTime endParseTime startParseTime)
  -- putStrLn "\n-- Debug elements: --\n"
  {- TODO: move this into a per-module step:
  TIO.writeFile (destPath </> "elements.txt") . T.pack . L.intercalate "\n" $ map (\(fp, ms) ->  fp <> ":\n" <> show ms <> "\n") allElements
  --putStrLn "\n--------------------------------\n"
  -}

  {- This will be handled on a per-module basis:
  !startGenTime <- getCurrentTime
  let
    eiClassDefs = map (\(f, elements) -> (f, Sq.genTableDefs elements)) allElements
  TIO.writeFile (destPath </> "tableDefs.sql") . T.pack . L.intercalate "\n" $ map (\(f, eiTableDefs) ->
      let
        msgs = foldl (\accum td ->
           case td of
             Left err -> accum <> err <> "\n"
             Right tableDef -> accum <> show tableDef <> "\n"
          ) "" eiTableDefs
      in
      "@[handlePyFiles] " <> f <> if msgs == "" then "" else ":\n" <> msgs
    ) eiClassDefs
  !endGenTime <- getCurrentTime
  putStrLn $ "@[handlePyFiles] time to process: " <> show (diffUTCTime endGenTime startGenTime)
  -}
  pure allElements
