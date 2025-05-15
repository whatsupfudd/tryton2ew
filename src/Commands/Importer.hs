{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use second" #-}
module Commands.Importer where

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
import qualified Parsing.Tryton as Tt
import qualified Parsing.Pot as Po


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
  putStrLn "--------------------------------"
  putStrLn $ "nbr potFiles: " <> show (length potFiles)
  handlePotFiles potFiles
  putStrLn "--------------------------------"
  putStrLn $ "nbr pyFiles:  " <> show (length pyFiles)
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
        if head name == '.' then
          pure []
        else
          case takeExtension path of
            ".xml" -> pure [TargetFile { kindTF = XmlFile, pathTF = path }]
            ".pot" -> pure [TargetFile { kindTF = PotFile, pathTF = path }]
            ".po" -> pure [TargetFile { kindTF = PotFile, pathTF = path }]
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
          !items = Tt.extractItems aDoc
          !defs = Tt.extractDefinitions aDoc
        in
        pure (items, defs, diffUTCTime endItemsTime startItemsTime)
      Nothing -> pure ([], [], 0)
  putStrLn $ "time to load xml Files: " <> show (sum (map (\(_, _, t) -> t) mbItems))

  let
    !tree = Tt.buildTree $ concatMap (\(items, _, _) -> items) mbItems
  Tt.printTree tree 0
  putStrLn "\n--------------------------------\n"

  !startDefsTime <- getCurrentTime
  let
    !allDefs = concatMap (\(_, defs, _) -> defs) mbItems
    !modelDefs =
      let
        !initMap = Mp.fromList [(l.modelDF, []) | l <- allDefs]
      in
      foldl (\accum d -> Mp.insertWith (++) d.modelDF [d] accum) initMap allDefs
  !endDefsTime <- getCurrentTime
  Tt.printDefinitions modelDefs
  pure ()


loadXmlFile :: FilePath -> IO (Maybe Document)
loadXmlFile filePath = do
  result <- try (X.readFile X.def filePath) :: IO (Either SomeException Document)
  case result of
      Right doc -> pure $ Just doc
      Left _    -> pure Nothing


handlePotFiles :: [FilePath] -> IO ()
handlePotFiles files = do
  !potFiles <- forM files $ \aFile -> do
    !potDoc <- Po.parseLocFileWithDiagnostics aFile
    pure (aFile, potDoc)
  putStrLn "\n-- Debug potFiles: --\n"
  mapM_ (\(f, ms) -> putStrLn $ f <> ":\n" <> show ms <> "\n") potFiles
  putStrLn "\n--------------------------------\n"


handlePyFiles :: [FilePath] -> IO ()
handlePyFiles files = do
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
