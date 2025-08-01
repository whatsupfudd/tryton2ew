{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}

module Tryton.Load where

import Control.Monad (forM, unless)
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
  , noLogicIO
-}

genModulesFromFolder :: ImporterOptions -> FilePath -> [TargetFile] -> IO (Either String [ModuleSrcTT])
genModulesFromFolder importOpts rootPath targetFiles =
  let
    !configFiles = [ tf.pathTF | tf <- targetFiles, tf.kindTF == CfgFile ]
    -- Upgrade to Text to use the isPrefixOf function later on:
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
                pure . Right $ readModuleContent moduleName aCfgFile configMap
  case lefts configRez of
    [] ->
      let
        (modules, (leftXml, leftPy, leftLocales)) = assignFilesToModules (xmlFiles, pyFiles, potFiles) (rights configRez)
      in do
      putStrLn $ "@[importerCmd] leftXml: " <> show leftXml
      putStrLn $ "@[importerCmd] leftPy: " <> show leftPy
      putStrLn $ "@[importerCmd] leftLocales: " <> show leftLocales
      pure $ Right modules
    errs -> pure $ Left $ L.intercalate "\n" errs


readModuleContent :: String -> FilePath -> Mp.Map Cf.OptionSpec String -> ModuleSrcTT
readModuleContent moduleName aCfgFile configMap =
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
  in
  ModuleSrcTT {
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


assignFilesToModules :: ([T.Text], [T.Text], [T.Text]) -> [ModuleSrcTT] -> ([ModuleSrcTT], ([T.Text], [T.Text], [T.Text]))
assignFilesToModules (xmlFiles, pyFiles, potFiles) initModules =
  let
    (leftOvers, consolidated) = foldl (\((xmlLO, pyLO, localesLO), modules) aModule ->
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
      ) ((xmlFiles, pyFiles, potFiles), []) initModules
  in
  (consolidated, leftOvers)


parseModule :: ImporterOptions -> ModuleSrcTT -> IO (Either String FullModuleTT)
parseModule importOpts aModule = do
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
      mbMainDef <- loadXmlFile (basePath </> targetFile)
      case mbMainDef of
        Nothing -> pure . Left $ "@[loadModule] no xml file for module: " <> aModule.nameMT
        Just mainDef ->
          let
            items = Xm.extractMenuItems mainDef
            modelViews = Xm.parseModelView mainDef
          in do

          viewsRez <- forM aModule.viewsMT $ \xmlFilePath -> do
            mbUiDef <- loadXmlFile (basePath </> "view" </> xmlFilePath)
            case mbUiDef of
              Nothing -> pure . Left $ "@[loadModule] loadXmlFile empty on: " <> aModule.nameMT <> "." <> xmlFilePath
              Just uiDef ->
                let
                  baseFileName = takeBaseName xmlFilePath
                  (views, _) = Xm.parseUiView baseFileName uiDef
                in do
                pure $ Right (T.pack baseFileName, views)
          (_, viewDefs) <- case lefts viewsRez of
            [] -> pure . Tp.consolidateDefinitions $ rights viewsRez
            errs -> do
              putStrLn $ "@[loadModule] error loading views: " <> L.intercalate "\n" errs
              pure . Tp.consolidateDefinitions $ rights viewsRez

          potFiles <- if importOpts.noLocalesIO then
            -- Still need to have the basic language definitions:
            let
              defaultPot = basePath </> "locale" </> aModule.nameMT <> ".pot"
            in
            loadPotFiles aModule.nameMT [defaultPot] importOpts.destPathIO
          else
            loadPotFiles aModule.nameMT [basePath </> aLocale | aLocale <- aModule.localesMT] importOpts.destPathIO

          logicElements <- if importOpts.noLogicIO then
              pure []
            else 
              loadPyFiles [basePath </> aPyFile | aPyFile <- aModule.logicMT] importOpts.destPathIO
          let
            eiClassDefs = map (\(f, elements) -> (f, Sq.genTableDefs elements)) logicElements

          tableDefs <- mapM (\(fileName, eiList) ->
                case lefts eiList of
                  [] -> pure (fileName, rights eiList)
                  errs -> do
                    putStrLn $ "@[loadModule] genTableDefs errs: " <> L.intercalate "\n" errs
                    pure (fileName, rights eiList)
              ) eiClassDefs
          -- TODO: give warnings on errors from each group that has been parsed.
          pure $ Right $ FullModuleTT {
            srcModuleFM = aModule
            , menusFM = items
            , actWinsFM = concatMap (\case
                     ModelDF clInstance -> [clInstance]
                     _ -> []
                  ) modelViews
            , xmlDefsFM = []
            , localesFM = case potFiles of
                Left err -> Mp.empty
                Right localeMap -> localeMap
            , viewDefsFM = Just viewDefs
            , logicFM = Mp.fromList [(T.pack aPyFile, elements) | (aPyFile, elements) <- logicElements]
            , sqlDefsFM = Mp.fromList $ concat [[(T.decodeUtf8 sqlTable.nameST, sqlTable) | sqlTable <- sqlTables ] | (filePath, sqlTables) <- tableDefs]
          }

consolidateModules :: [FullModuleTT] -> Either String TrytonApp
consolidateModules modules =
  let
    !tree = Tp.buildMenuTree $ concatMap menusFM modules
    allLocales = Mp.fromList $ foldl (\accum aModule ->
      case Mp.lookup "en" aModule.localesFM of
        Nothing -> accum
        Just locEntries -> (T.encodeUtf8 . T.pack $ aModule.srcModuleFM.nameMT, locEntries) : accum
      ) [] modules
    instancesByKind = foldl (\aAccum aModule ->
        foldl (\bAccum aMInstance ->
            Mp.insertWith (++) aMInstance.modelDF [aMInstance] bAccum
          ) aAccum aModule.actWinsFM
      ) Mp.empty modules
  in
  Right $ TrytonApp {
    modulesTA = modules
    , menuTreeTA = tree
    , localesTA = Mp.singleton "en" allLocales
    , instancesByKindTA = instancesByKind
  }
  {-
  -}

{-
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
    (mInstances, viewDefs) = Tp.consolidateDefinitions $ map snd mbItems
  -- putStrLn $ "@[handleXmlFiles] modelDefs: " <> show modelDefs
  -- putStrLn $ "@[handleXmlFiles] viewDefs: " <> show viewDefs
  !endDefsTime <- getCurrentTime
  --putStrLn $ "@[handleXmlFiles] time to make viewDefs: " <> show (diffUTCTime endDefsTime startDefsTime)
  Tp.printModelInstances mInstances destPath
  Tp.printViewErrs viewDefs destPath
  pure (tree, mInstances, viewDefs)
-}

loadXmlFile :: FilePath -> IO (Maybe Document)
loadXmlFile filePath = do
  result <- try (X.readFile X.def filePath) :: IO (Either SomeException Document)
  case result of
      Right doc -> pure $ Just doc
      Left _    -> pure Nothing


loadPotFiles :: String -> [FilePath] -> FilePath -> IO (Either String (Mp.Map Bs.ByteString [Po.LocEntry]))
loadPotFiles moduleName files destPath = do
  potFiles <- forM files $ \aFile -> do
    !locFile <- Po.parseLocFileWithDiagnostics aFile
    case locFile of
      Left err -> pure $ Left $ "@[loadPotFiles] parseLocFileWithDiagnostics err: " <> show err
      Right locFile ->
        if takeBaseName aFile == moduleName then
           pure $ Right ("en", locFile.entriesFI)
        else
          pure $ Right (T.encodeUtf8 . T.pack $ takeBaseName aFile, locFile.entriesFI)
  case lefts potFiles of
    [] ->
      pure $ Right $ Mp.fromList (rights potFiles)
    errs -> pure $ Left $ L.intercalate "\n" errs

{-
handlePotFiles :: String -> [FilePath] -> FilePath -> IO Po.LocaleDefs
handlePotFiles moduleName files destPath = do
  !startParseTime <- getCurrentTime
  !potFiles <- forM files $ \aFile -> do
    !potDoc <- Po.parseLocFileWithDiagnostics aFile
    let
      -- moduleName = takeDirectory aFile
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
-}


loadPyFiles :: [FilePath] -> FilePath -> IO [(FilePath, [Py.LogicElement])]
loadPyFiles files destPath = do
  forM files $ \aFile -> do
    !elements <- Py.extractElements aFile
    pure (aFile, elements)

  -- Old code:
  -- !startParseTime <- getCurrentTime
  -- !endParseTime <- getCurrentTime
  -- putStrLn $ "@[loadPyFiles] time to parse: " <> show (diffUTCTime endParseTime startParseTime)
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
      "@[loadPyFiles] " <> f <> if msgs == "" then "" else ":\n" <> msgs
    ) eiClassDefs
  !endGenTime <- getCurrentTime
  putStrLn $ "@[loadPyFiles] time to process: " <> show (diffUTCTime endGenTime startGenTime)
  -}
