{-# LANGUAGE DerivingStrategies #-}

module Options.Cli where

import Data.Text (Text)
import Options.Applicative


newtype EnvOptions = EnvOptions {
    appHome :: Maybe Text
  }

data CliOptions = CliOptions {
  debug :: Maybe Int
  , configFile :: Maybe FilePath
  , job :: Maybe Command
 }
 deriving stock (Show)

data GlobalOptions = GlobalOptions {
  confPathGO :: String
  , debugGO :: String
  }

data ImporterOptions = ImporterOptions {
  inPathIO :: FilePath
  , destPathIO :: FilePath
  , schemaIO :: Bool
  , dataPrepIO :: Bool
  , noAppIO :: Bool
  , noLocalesIO :: Bool
  }
 deriving (Show)

data Command =
  HelpCmd
  | VersionCmd
  | ImporterCmd ImporterOptions
  deriving (Show)


parseCliOptions :: IO (Either String CliOptions)
parseCliOptions =
  Right <$> execParser parser

parser :: ParserInfo CliOptions
parser =
  info (helper <*> argumentsP) $
    fullDesc <> progDesc "extractor." <> header "extractor - ."


argumentsP :: Parser CliOptions
argumentsP = do
  buildOptions <$> globConfFileDef <*> hsubparser commandDefs
  where
    buildOptions :: GlobalOptions -> Command -> CliOptions
    buildOptions globs cmd =
      let
        mbConfPath = case globs.confPathGO of
          "" -> Nothing
          aValue -> Just aValue
        mbDebug = case globs.debugGO of
          "" -> Nothing
          aValue -> Just (read aValue :: Int)
      in
      CliOptions {
        debug = mbDebug
        , configFile = mbConfPath
        , job = Just cmd
      }


globConfFileDef :: Parser GlobalOptions
globConfFileDef =
  GlobalOptions <$>
    strOption (
      long "config"
      <> short 'c'
      <> metavar "extractorCONF"
      <> value ""
      <> showDefault
      <> help "Global config file (default is ~/.extractor/config.yaml)."
    )
    <*>
    strOption (
      long "debug"
      <> short 'd'
      <> metavar "DEBUGLVL"
      <> value ""
      <> showDefault
      <> help "Global debug state."
    )


commandDefs :: Mod CommandFields Command
commandDefs =
  let
    cmdArray = [
      ("help", pure HelpCmd, "Help about any command.")
      , ("version", pure VersionCmd, "Shows the version number of importer.")
      , ("import", ImporterCmd <$> importerOpts, "Loads up the Tryton definitions from a directory and converts them to an EasyWordy application.")
      ]
    headArray = head cmdArray
    tailArray = tail cmdArray
  in
    foldl (\accum aCmd -> cmdBuilder aCmd <> accum) (cmdBuilder headArray) tailArray
  where
    cmdBuilder (label, cmdDef, desc) =
      command label (info cmdDef (progDesc desc))

importerOpts :: Parser ImporterOptions
importerOpts =
  ImporterOptions <$> strArgument (metavar "FILE" <> help "Directory containing the Tryton definitions.")
              <*> strArgument (metavar "FILE" <> help "Directory to save the EW app in.")
              <*> switch (
                  long "schema"
                  <> short 's'
                  <> help "Generate DB schemas from Python definitions."
                )
              <*> switch (
                  long "dataprep"
                  <> short 'p'
                  <> help "Generate DB data prep from XML definitions."
                )
              <*> switch (
                  long "noapp"
                  <> help "Do not generate the EasyWordy Wapp."
                )
              <*> switch (
                  long "nopot"
                  <> help "Do not parse the locales (.po, .pot) files."
                )