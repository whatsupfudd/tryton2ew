{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use second" #-}
module Commands.Importer where

import Control.Applicative         ((<|>))
import Control.Monad               (forM, forM_, mapM, unless, when)
import Control.Exception           (try, SomeException)

import qualified Data.ByteString as Bs
import Data.Either (partitionEithers, rights)
import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))

import Text.XML (Document(..), Element(..), Node(..), Prologue(..), renderText)
import qualified Text.XML as X
import Text.XML.Cursor (Cursor, attribute, child, content
                        , element, fromDocument, node, ($//), ($/), (>=>), (&/))

import qualified Options.Runtime as Rto
import qualified Options.Cli as Cli
import qualified Parsing.Python as Py
import qualified Generation.Sql as Sq
import qualified Parsing.Xml as Xm
import qualified Parsing.Pot as Po
import qualified Generation.EwTypes as Ew
import qualified Generation.EasyWordy as Ew


data TargetFileKind =
    XmlFile
  | PotFile
  | PyFile
  deriving (Show, Eq)


data TargetFile = TargetFile {
    kindTF :: !TargetFileKind
  , pathTF :: !FilePath
  }


type XmlDefs = ([Xm.MenuItem], Mp.Map T.Text [Xm.ClassInstance], Xm.ViewDefs)


importerCmd :: Cli.ImporterOptions -> Rto.RunOptions -> IO ()
importerCmd importOpts rtOpts =
  let
    testing = False
  in
  if testing then
    putStrLn "@[importerCmd] nothing to test."
  else do
    targetFiles <- getTargetFiles importOpts.inPathIO

    (xmlDefs, locales) <- if importOpts.noAppIO then
        pure (([], Mp.empty, Xm.emptyViewDefs), Mp.empty)
      else
        let
          !xmlFiles = [ tf.pathTF | tf <- targetFiles, tf.kindTF == XmlFile ]
          !potFiles = [ tf.pathTF | tf <- targetFiles, tf.kindTF == PotFile ]
        in do
        putStrLn $ "nbr xmlFiles: " <> show (length xmlFiles)
        xmlRez <- handleXmlFiles xmlFiles importOpts.destPathIO
        putStrLn $ "nbr potFiles: " <> show (length potFiles)
        potRez <- if importOpts.noLocalesIO then
          pure Mp.empty
        else
          handlePotFiles potFiles importOpts.destPathIO
        pure (xmlRez, potRez)
    let
      (_, clInstances, _) = xmlDefs

    let
      !pyFiles = [ tf.pathTF | tf <- targetFiles, tf.kindTF == PyFile ]
    putStrLn $ "nbr pyFiles:  " <> show (length pyFiles)
    logicDefs <- handlePyFiles pyFiles importOpts.destPathIO
    genStartTime <- getCurrentTime

    unless importOpts.noAppIO $ do
      rez <- Ew.generateApp importOpts.destPathIO xmlDefs locales logicDefs
      pure ()

    when importOpts.schemaIO $ do
      rez <- Sq.genSchemas importOpts.destPathIO logicDefs
      case rez of
        Left err -> putStrLn $ "@[importerCmd] genSchemas err: " <> err
        Right _ -> pure ()
    when importOpts.bootstrapIO $
      Sq.genBootstrap importOpts.destPathIO clInstances

    genEndTime <- getCurrentTime
    putStrLn $ "@[importerCmd] time to generate: " <> show (diffUTCTime genEndTime genStartTime)
    pure ()


getTargetFiles :: FilePath -> IO [TargetFile]
getTargetFiles !dir = do
    !entries <- listDirectory dir
    !paths <- forM entries $ \name -> do
      let path = dir </> name
      !isDir <- doesDirectoryExist path
      if isDir then
        getTargetFiles path
      else
        if head name == '.' then
          pure []
        else
          case takeExtension path of
            ".xml" -> pure [TargetFile { kindTF = XmlFile, pathTF = path }]
            ".pot" -> pure [TargetFile { kindTF = PotFile, pathTF = path }]
            ".po" -> pure [TargetFile { kindTF = PotFile, pathTF = path }]
            ".py" -> pure [TargetFile { kindTF = PyFile, pathTF = path }]
            _ -> pure []
    pure (concat paths)


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
  putStrLn $ "@[handleXmlFiles] time to load xml files: " <> show (diffUTCTime endItemsTime startItemsTime)

  let
    !tree = Xm.buildMenuTree $ concatMap fst mbItems
  Xm.printMenuTree tree 0 destPath
  -- putStrLn "\n--------------------------------\n"

  !startDefsTime <- getCurrentTime
  let
    (classInstances, viewDefs) = Xm.processDefinitions $ map snd mbItems
  -- putStrLn $ "@[handleXmlFiles] modelDefs: " <> show modelDefs
  -- putStrLn $ "@[handleXmlFiles] viewDefs: " <> show viewDefs
  !endDefsTime <- getCurrentTime
  putStrLn $ "@[handleXmlFiles] time to make viewDefs: " <> show (diffUTCTime endDefsTime startDefsTime)
  Xm.printClassInstances classInstances destPath
  Xm.printViewErrs viewDefs destPath
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
  putStrLn $ "@[handlePotFiles] time to parse: " <> show (diffUTCTime endParseTime startParseTime)

  -- putStrLn "\n-- Debug potFiles: --\n"
  TIO.writeFile (destPath </> "potFiles.txt") . T.pack . L.intercalate "\n" $ map (\(mName, fileName, rez) ->
      T.unpack (mName <> ":" <> fileName) <> "\n"
      <> case rez of
            Left err -> "err: " <> show err
            Right potDoc -> show potDoc
    ) potFiles
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
  putStrLn $ "@[handlePotFiles] time to process: " <> show (diffUTCTime endProcTime startProcTime)
  TIO.writeFile (destPath </> "reorgLocales.txt") $ T.pack (show reorgLocales)
  pure reorgLocales


handlePyFiles :: [FilePath] -> FilePath -> IO [(FilePath, [Py.LogicElement])]
handlePyFiles files destPath = do
  !startParseTime <- getCurrentTime
  !allElements <- forM files $ \aFile -> do
    !elements <- Py.extractElements aFile
    pure (aFile, elements)
  !endParseTime <- getCurrentTime
  putStrLn $ "@[handlePyFiles] time to parse: " <> show (diffUTCTime endParseTime startParseTime)
  -- putStrLn "\n-- Debug elements: --\n"
  TIO.writeFile (destPath </> "elements.txt") . T.pack . L.intercalate "\n" $ map (\(fp, ms) ->  fp <> ":\n" <> show ms <> "\n") allElements
  --putStrLn "\n--------------------------------\n"

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
  pure allElements
