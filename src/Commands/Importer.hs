{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use second" #-}
module Commands.Importer where

import Control.Monad (forM_,forM, unless, when)

import Data.Either (rights, lefts)
import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

import qualified Options.Runtime as Rto
import qualified Options.Cli as Cli
import qualified Generation.Sql as Sq
import qualified Generation.DataPrep as Dp
import qualified Parsing.Xml as Xm
import qualified Tryton.Types as Tm
import qualified Tryton.Load as Tl
import qualified Tryton.Process as Tp
import qualified Generation.EwTypes as Ew
import qualified Generation.EasyWordy as Ew


importerCmd :: Cli.ImporterOptions -> Rto.RunOptions -> IO ()
importerCmd importOpts rtOpts =
  let
    testing = False
  in
  if testing then
    putStrLn "@[importerCmd] nothing to test."
  else do
    targetFiles <- getTargetFiles importOpts.inPathIO
    eiModules <- Tl.genModulesFromFolder importOpts importOpts.inPathIO targetFiles
    case eiModules of
      Left errs -> putStrLn $ "@[importerCmd] loadConfiguration errs: " <> errs
      Right modules -> do
        loadRez <- mapM (Tl.parseModule importOpts) modules
        case lefts loadRez of
          [] -> do
            forM_ (rights loadRez) $ \aModule -> do
              putStrLn $ "@[importerCmd] module: " <> aModule.srcModuleFM.nameMT
                <> ", menus: " <> show (length aModule.menusFM)
                <> ", actWins: " <> show (length aModule.actWinsFM)
                <> ", xmlDefs: " <> show (length aModule.xmlDefsFM)
                -- <> "locales: " <> show aModule.localesFM
                <> ", viewDefs: " <> show (maybe (0,0) Tm.lengthViewDefs aModule.viewDefsFM)
                <> ", logic: " <> show (length aModule.logicFM)
                <> ", sqlDefs: " <> show (length aModule.sqlDefsFM)
            let
              (nbrMenus, nbrActWins, nbrXmlDefs, nbrViewDefs, nbrLogic, nbrSqlDefs) =
                foldr (\aModule (nbrMenus, nbrActWins, nbrXmlDefs, nbrViewDefs, nbrLogic, nbrSqlDefs) ->
                  (nbrMenus + length aModule.menusFM,
                  nbrActWins + length aModule.actWinsFM,
                  nbrXmlDefs + length aModule.xmlDefsFM,
                  n2Sum nbrViewDefs (maybe (0,0) Tm.lengthViewDefs aModule.viewDefsFM),
                  nbrLogic + length aModule.logicFM,
                  nbrSqlDefs + length aModule.sqlDefsFM)) (0,0,0,(0,0),0,0) (rights loadRez)
            putStrLn $ "@[importerCmd] total: " <> show (nbrMenus, nbrActWins, nbrXmlDefs, nbrViewDefs, nbrLogic, nbrSqlDefs)
            case Tl.consolidateModules (rights loadRez) of
              Left errs -> putStrLn $ "@[importerCmd] consolidateModules errs: " <> errs
              Right trytonApp -> do
                -- putStrLn $ "@[importerCmd] app: " <> show trytonApp
                Tp.printMenuTree trytonApp.menuTreeTA 0 (importOpts.destPathIO </> "menuTree.txt")
                eiRez <- Ew.generateApp importOpts.destPathIO trytonApp
                case eiRez of
                  Left errs -> putStrLn $ "@[importerCmd] generateApp errs: " <> errs
                  Right () -> pure ()
          errs -> putStrLn $ "@[importerCmd] loadModule errs: " <> L.intercalate "\n" errs

n6Sum :: (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int)
n6Sum (a,b,c,d,e,f) (g,h,i,j,k,l) = (a+g,b+h,c+i,d+j,e+k,f+l)
n2Sum :: (Int, Int) -> (Int, Int) -> (Int, Int)
n2Sum (a,b) (c,d) = (a+c,b+d)

{-
importerCmdV0 :: Cli.ImporterOptions -> Rto.RunOptions -> IO ()
importerCmdV0 importOpts rtOpts =
  let
    testing = False
  in
  if testing then
    putStrLn "@[importerCmd] nothing to test."
  else do
    targetFiles <- getTargetFiles importOpts.inPathIO

    -- Old approach:
    (xmlDefs, locales) <- if importOpts.noAppIO then
        pure (([], Mp.empty, Tm.emptyViewDefs), Mp.empty)
      else
        let
          !xmlFiles = [ tf.pathTF | tf <- targetFiles, tf.kindTF == Tm.XmlFile ]
          !potFiles = [ tf.pathTF | tf <- targetFiles, tf.kindTF == Tm.PotFile ]
        in do
        putStrLn $ "nbr xmlFiles: " <> show (length xmlFiles)
        xmlStartTime <- getCurrentTime
        xmlRez <- Tl.handleXmlFiles xmlFiles importOpts.destPathIO
        xmlEndTime <- getCurrentTime
        putStrLn $ "@[importerCmd] handleXmlFiles time: " <> show (diffUTCTime xmlEndTime xmlStartTime)
        putStrLn $ "nbr potFiles: " <> show (length potFiles)
        locDefs <- if importOpts.noLocalesIO then
          pure Mp.empty
        else do
          potStartTime <- getCurrentTime
          potRez <- Tl.handlePotFiles potFiles importOpts.destPathIO
          potEndTime <- getCurrentTime
          putStrLn $ "@[importerCmd] handlePotFiles time: " <> show (diffUTCTime potEndTime potStartTime)
          pure potRez
        pure (xmlRez, locDefs)
    let
      (_, clInstances, _) = xmlDefs


    let
      !pyFiles = [ tf.pathTF | tf <- targetFiles, tf.kindTF == Tm.PyFile ]
    putStrLn $ "nbr pyFiles:  " <> show (length pyFiles)
    pyStartTime <- getCurrentTime
    logicDefs <- Tl.handlePyFiles pyFiles importOpts.destPathIO
    pyEndTime <- getCurrentTime
    putStrLn $ "@[importerCmd] handlePyFiles time: " <> show (diffUTCTime pyEndTime pyStartTime)
    let
      eiTableDefs = concatMap Sq.genTableDefs [ snd anElement | anElement <- logicDefs ]
    case rights eiTableDefs of
      [] -> putStrLn $ "@[importerCmd] no table, genTableDefs errs: " <> L.intercalate "\n" (lefts eiTableDefs)
      tableDefs -> do
        let
          tableMap = Mp.fromList [(T.decodeUtf8 aTable.nameST, aTable) | aTable <- tableDefs]

        unless importOpts.noAppIO $ do
          genStartTime <- getCurrentTime
          rez <- Ew.generateAppV0 importOpts.destPathIO xmlDefs tableMap locales logicDefs
          genEndTime <- getCurrentTime
          putStrLn $ "@[importerCmd] generateApp time: " <> show (diffUTCTime genEndTime genStartTime)
          pure ()

        if importOpts.schemaIO then do
          Sq.genSchemas importOpts.destPathIO tableDefs
          when importOpts.dataPrepIO $
            if importOpts.noAppIO then
              putStrLn "@[importerCmd] data prep needs app parsing, don't use --noapp."
            else
              Dp.genBootstrap importOpts.destPathIO tableMap clInstances
        else when importOpts.dataPrepIO $
          let
            eiTableDefs = concatMap Sq.genTableDefs [ snd anElement | anElement <- logicDefs ]
          in
          case rights eiTableDefs of
            [] -> putStrLn $ "@[importerCmd] genTableDefs errs: " <> L.intercalate "\n" (lefts eiTableDefs)
            sqlTables ->
              Dp.genBootstrap importOpts.destPathIO tableMap clInstances
    pure ()
-}

getTargetFiles :: FilePath -> IO [Tm.TargetFile]
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
            ".xml" -> pure [Tm.TargetFile { kindTF = Tm.XmlFile, pathTF = path }]
            ".pot" -> pure [Tm.TargetFile { kindTF = Tm.PotFile, pathTF = path }]
            ".po" -> pure [Tm.TargetFile { kindTF = Tm.PotFile, pathTF = path }]
            ".py" -> pure [Tm.TargetFile { kindTF = Tm.PyFile, pathTF = path }]
            ".cfg" -> pure [Tm.TargetFile { kindTF = Tm.CfgFile, pathTF = path }]
            _ -> pure []
    pure (concat paths)



