{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use second" #-}
module Commands.Importer where

import Control.Applicative         ((<|>))
import Control.Monad               (forM, forM_, mapM)
import Control.Exception           (try, SomeException)

import qualified Data.ByteString as Bs
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
import qualified Parsing.Python as Py
import qualified Generation.Sql as Sq
import qualified Parsing.Tryton as Tt
import qualified Parsing.Pot as Po
import qualified Generation.EwTypes as Ew
import qualified Generation.EasyWordy as Ew
import Data.Either (partitionEithers)


data TargetFileKind =
    XmlFile
  | PotFile
  | PyFile
  deriving (Show, Eq)


data TargetFile = TargetFile {
    kindTF :: !TargetFileKind
  , pathTF :: !FilePath
  }


type UiDefs = ([Tt.MenuItem], Mp.Map T.Text [Tt.Definition])


menuFinderCmd :: FilePath -> FilePath -> Rto.RunOptions -> IO ()
menuFinderCmd srcPath destPath rtOpts =
  let
    testing = False
  in
  if testing then
    putStrLn $ "@[menuFinderCmd] nothing to test."
  else do
    targetFiles <- getTargetFiles srcPath
    let
      !xmlFiles = [ tf.pathTF | tf <- targetFiles, tf.kindTF == XmlFile ]
      !potFiles = [ tf.pathTF | tf <- targetFiles, tf.kindTF == PotFile ]
      !pyFiles  = [ tf.pathTF | tf <- targetFiles, tf.kindTF == PyFile ]
    putStrLn $ "nbr xmlFiles: " <> show (length xmlFiles)
    uiDefs <- handleXmlFiles xmlFiles destPath
    {-
    let
      defs = snd uiDefs
    case Mp.lookup "ir.ui.icon" defs of
      Nothing -> pure ()
      Just iconDefs ->
        let
          iconMaps = Ew.buildIconDefMap (Just iconDefs)
        in
          putStrLn $ "@[menuFinderCmd] iconMap: " <> show iconMaps
    -}
    putStrLn $ "nbr potFiles: " <> show (length potFiles)
    locales <- handlePotFiles potFiles destPath
    putStrLn $ "nbr pyFiles:  " <> show (length pyFiles)
    dbDefs <- handlePyFiles pyFiles destPath
    rez <- Ew.generateApp destPath uiDefs locales dbDefs
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


handleXmlFiles :: [FilePath] -> FilePath -> IO UiDefs
handleXmlFiles !files destPath = do
  !mbItems <- forM files $ \aFile -> do
    !startItemsTime <- getCurrentTime
    !xmlDoc <- loadXmlFile aFile
    !endItemsTime <- getCurrentTime
    case xmlDoc of
      Just aDoc ->
        let
          !items = Tt.extractItems aDoc
          !defs = Tt.extractDefinitions aDoc
        in
        pure (items, defs, diffUTCTime endItemsTime startItemsTime)
      Nothing -> pure ([], [], 0)
  putStrLn $ "time to load xml Files: " <> show (sum (map (\(_, _, t) -> t) mbItems))

  let
    !tree = Tt.buildTree $ concatMap (\(items, _, _) -> items) mbItems
  Tt.printTree tree 0 destPath
  -- putStrLn "\n--------------------------------\n"

  !startDefsTime <- getCurrentTime
  let
    !allDefs = concatMap (\(_, defs, _) -> defs) mbItems
    !modelDefs =
      let
        !initMap = Mp.fromList [(l.modelDF, []) | l <- allDefs]
      in
      foldl (\accum d -> Mp.insertWith (++) d.modelDF [d] accum) initMap allDefs
  !endDefsTime <- getCurrentTime
  putStrLn $ "time to make model map: " <> show (diffUTCTime endDefsTime startDefsTime)
  Tt.printDefinitions modelDefs destPath
  pure (tree, modelDefs)


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
  putStrLn $ "time to parse potFiles: " <> show (diffUTCTime endParseTime startParseTime)

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
          Right locEntries -> (errs, (mName, locale, locEntries) : corrects)
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
        ) (Mp.empty :: Mp.Map T.Text (Mp.Map T.Text [Po.LocEntry])) locEntries
  !endProcTime <- getCurrentTime
  putStrLn $ "time to process potFiles: " <> show (diffUTCTime endProcTime startProcTime)
  TIO.writeFile (destPath </> "reorgLocales.txt") $ T.pack (show reorgLocales)
  pure reorgLocales


handlePyFiles :: [FilePath] -> FilePath -> IO [Ew.TableDef]
handlePyFiles files destPath = do
  !startParseTime <- getCurrentTime
  !allElements <- forM files $ \aFile -> do
    !elements <- Py.extractElements aFile
    pure (aFile, elements)
  !endParseTime <- getCurrentTime
  putStrLn $ "time to parse pyFiles: " <> show (diffUTCTime endParseTime startParseTime)
  -- putStrLn "\n-- Debug elements: --\n"
  TIO.writeFile (destPath </> "elements.txt") . T.pack . L.intercalate "\n" $ map (\(fp, ms) ->  fp <> ":\n" <> show ms <> "\n") allElements
  --putStrLn "\n--------------------------------\n"

  !startGenTime <- getCurrentTime
  let
    eiFileDefs = map (\(f, elements) -> (f, Sq.genTableDef elements)) allElements
  TIO.writeFile (destPath </> "tableDefs.sql") . T.pack . L.intercalate "\n" $ map (\(f, eiTableDefs) ->
      let
        msgs = foldl(\accum td ->
           case td of
             Left err -> accum <> err <> "\n"
             Right tableDef -> accum <> show tableDef <> "\n"
          ) "" eiTableDefs
      in
      "@[handlePyFiles] " <> f <> if msgs == "" then "" else ":\n" <> msgs
    ) eiFileDefs
  !endGenTime <- getCurrentTime
  putStrLn $ "time to gen tableDefs: " <> show (diffUTCTime endGenTime startGenTime)
  pure []
