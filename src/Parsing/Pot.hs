module Parsing.Pot where

import Control.Applicative ((<|>), many)
import Control.Monad (void)

import qualified Data.List as L
import qualified Data.List.NonEmpty as Ne
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)

import System.IO (hPutStrLn, stderr)
import System.FilePath (takeFileName)

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as CL


type Parser = M.Parsec Void Text

-- LocaleDefs : <locale> -> ( <module> -> [LocEntry] )
type LocaleDefs = Map.Map T.Text (Map.Map T.Text [LocEntry])

-- | Represents a single localization entry in the .po/.pot file
data LocEntry = LocEntry
  { contextEN :: !Text  -- ^ The msgctxt field (module ID)
  , keyEN     :: !Text  -- ^ The msgid field (key)
  , valueEN   :: !Text  -- ^ The msgstr field (value)
  } deriving (Show, Eq)

-- | Represents a complete localization file with header and entries
data LocFile = LocFile
  { headerFI  :: !Text      -- ^ The header content (usually content type info)
  , entriesFI :: ![LocEntry] -- ^ The list of localization entries
  } deriving (Show, Eq)

-- | Parses a complete .po/.pot file with detailed error reporting
parseLocFile :: FilePath -> IO (Either (M.ParseErrorBundle Text Void) LocFile)
parseLocFile filePath = do
  content <- TIO.readFile filePath
  return $ M.runParser locFileParser filePath content


parseLocFileWithDiagnostics :: FilePath -> IO (Either (M.ParseErrorBundle Text Void) LocFile)
parseLocFileWithDiagnostics filePath = do
  result <- parseLocFile filePath
  case result of
    Right locFile -> return $ Right locFile
    Left err -> do
      -- Extract the error position from the error bundle
      let errOffset = case M.bundleErrors err of
                        (M.TrivialError offset _ _) Ne.:| _ -> offset
                        _ -> 0
      -- Print detailed diagnostics
      putStrLn $ "Parsing failed for " <> filePath <> ". Detailed diagnostics:"
      diagnosePosition filePath errOffset
      putStrLn "\nDetailed error message:"
      printParseError err
      return $ Left err


-- | Parse a string directly (useful for testing)
parseLocString :: String -> Text -> Either (M.ParseErrorBundle Text Void) LocFile
parseLocString = M.runParser locFileParser

-- | Parser for an entire localization file
locFileParser :: Parser LocFile
locFileParser = do
  sc
  header <- M.option "" headerParser  -- Optional header
  entries <- many entryParser
  M.eof
  return $ LocFile header entries

-- | Parser for the file header
headerParser :: Parser Text
headerParser = M.label "file header" $ do
  -- First, check for a header
  hasHeader <- M.option False (C.char '#' *> pure True)
  
  if hasHeader
    then do
      -- Parse the rest of the header
      C.eol
      C.string "msgid" *> scNoComments *> C.string "\"\"" *> scNoComments
      C.string "msgstr" *> scNoComments
      headerContent <- quotedStringParser
      scNoComments
      return headerContent
    else
      return ""  -- No header found, return empty string


-- | Parser for a single entry in the file
entryParser :: Parser LocEntry
entryParser = M.label "localization entry" $ do
  sc
  ctx <- contextParser
  key <- keyParser
  val <- valueParser
  sc  -- Skip whitespace between entries
  return $ LocEntry ctx key val

-- | Parser for msgctxt field
contextParser :: Parser Text
contextParser = M.label "msgctxt field" $ do
  C.string "msgctxt" *> sc
  quotedStringParser

-- | Parser for msgid field
keyParser :: Parser Text
keyParser = M.label "msgid field" $ do
  C.string "msgid" *> sc
  quotedStringParser

-- | Parser for msgstr field
valueParser :: Parser Text
valueParser = M.label "msgstr field" $ do
  C.string "msgstr" *> sc
  quotedStringParser

-- | Parser for quoted strings, which can span multiple lines
quotedStringParser :: Parser Text
quotedStringParser = M.label "quoted string" $ do
  strings <- M.some quotedPart
  return $ T.concat strings
  where
    quotedPart = do
      C.char '"'
      -- content <- M.try escapedStringContent <|> simpleStringContent
      content <- parseStringContent
      C.char '"' M.<?> "closing quote (found unclosed string, possibly unescaped backslash before this point)"
      sc
      return content

    {-
    simpleStringContent = M.takeWhileP Nothing (/= '"')    
    -- Parser for string content that handles escaped characters
    escapedStringContent = do
      -- Look for a backslash followed by a quote or another backslash
      let validEscapes = "\\\"" :: String -- Valid characters after a backslash
      
      -- Build up the string piece by piece, handling escapes
      let parseChar = do
            c <- M.anySingle
            case c of
              '\\' -> do
                next <- M.satisfy (`elem` validEscapes) M.<?> "valid escape sequence after backslash (\\n, \\t, \\\", or \\\\)"
                return $ if next == '"' then '\"' else '\\'
              _ -> return c
                
      T.pack <$> M.many (M.try parseChar) <* M.lookAhead (C.char '"')
    -}

    parseStringContent = do
      -- Build the string character by character, handling escapes
      chars <- many parseChar
      return $ T.pack chars
    
    -- Parse a single character, handling escapes
    parseChar :: Parser Char
    parseChar = M.try parseEscaped <|> parseNormal
    
    -- Parse an escaped character sequence
    parseEscaped :: Parser Char
    parseEscaped = do
      C.char '\\'  -- A single backslash as escape character
      c <- M.satisfy (`L.elem` ("\"\\nrt" :: String)) M.<?> "valid escape character (\\, \", n, r, or t)"
      return $ case c of
        '\\' -> '\\'  -- Backslash
        '"'  -> '"'   -- Quote
        'n'  -> '\n'  -- Newline
        'r'  -> '\r'  -- Carriage return
        't'  -> '\t'  -- Tab
        _    -> c     -- Should not happen due to the satisfy
    
    -- Parse a normal (non-escaped) character
    parseNormal :: Parser Char
    parseNormal = M.noneOf ['\\', '"']

scNoComments :: Parser ()
scNoComments = CL.space 
     C.space1    -- Normal whitespace
     M.empty     -- No line comments
     M.empty     -- No multi-line comments

-- | Comment consumer that skips over comment lines,
-- but preserves the special header comment line that contains only "#"
commentLine :: Parser ()
commentLine = M.try $ do
  M.notFollowedBy (C.string "#\n" <|> C.string "#\r\n")  -- Don't consume the header '#' line
  C.char '#'
  M.skipMany (M.satisfy (/= '\n'))
  void C.eol <|> M.eof
  return ()

-- | Space consumer that handles whitespace and typical comments
-- but preserves the special header comment
sc :: Parser ()
sc = do
  scNoComments
  M.skipMany (commentLine *> scNoComments)

-- | Convert entries to a Map for efficient lookups
entriesToMap :: [LocEntry] -> Map (Text, Text) Text
entriesToMap = Map.fromList . map (\e -> ((contextEN e, keyEN e), valueEN e))

-- | Look up a translation by context and key
lookupTranslation :: LocFile -> Text -> Text -> Maybe Text
lookupTranslation locFile ctx key = 
  Map.lookup (ctx, key) (entriesToMap (entriesFI locFile))


diagnosePosition :: FilePath -> Int -> IO ()
diagnosePosition filePath offset = do
  content <- TIO.readFile filePath
  let vicinity = 50  -- Show 50 chars before and after the error
      start = max 0 (offset - vicinity)
      end = min (T.length content) (offset + vicinity)
      before = T.take vicinity (T.drop start content)
      after = T.take vicinity (T.drop offset content)
      
      -- Count line numbers up to the error position
      lines = T.lines (T.take offset content)
      lineNum = length lines
      
      -- Get the line containing the error
      errorLine = if null lines then "" else last lines
      
  putStrLn $ "Error near line " ++ show lineNum
  putStrLn "Content before error point:"
  putStrLn $ T.unpack before
  putStrLn ">>> ERROR POSITION <<<"
  putStrLn "Content after error point:"
  putStrLn $ T.unpack after
  putStrLn "\nThe problematic line appears to be:"
  putStrLn $ T.unpack errorLine



-- | Format the file header
formatHeader :: Text -> Text
formatHeader headerContent =
  T.unlines
    [ "#"
    , "msgid \"\""
    , "msgstr " <> (if T.null headerContent then "\"\"" else headerContent)
    ]

-- | Format a localization entry back to the .po/.pot format
formatEntry :: LocEntry -> Text
formatEntry entry = T.unlines
  [ "msgctxt " <> formatString (contextEN entry)
  , "msgid " <> formatString (keyEN entry)
  , "msgstr " <> formatString (valueEN entry)
  , ""  -- Empty line between entries
  ]
  where
    formatString :: Text -> Text
    formatString s
      | T.null s = "\"\""
      | T.length s > 60 = formatMultiline s
      | otherwise = "\"" <> s <> "\""
    
    formatMultiline :: Text -> Text
    formatMultiline s = T.unlines $ map (\line -> "\"" <> line <> "\"") (splitTextLines s 60)
    
    splitTextLines :: Text -> Int -> [Text]
    splitTextLines txt maxLen
      | T.length txt <= maxLen = [txt]
      | otherwise = let (first, rest) = T.splitAt maxLen txt
                    in first : splitTextLines rest maxLen

-- | Save a localization file
saveLocFile :: FilePath -> LocFile -> IO ()
saveLocFile filePath locFile = do
  let headerText = formatHeader (headerFI locFile)
      entriesText = T.concat $ map formatEntry (entriesFI locFile)
      content = headerText <> entriesText
  TIO.writeFile filePath content

-- | Merge two localization files (e.g., to update translations)
mergeLocFiles :: LocFile -> LocFile -> LocFile
mergeLocFiles baseFile newFile =
  let baseMap = entriesToMap (entriesFI baseFile)
      newMap = entriesToMap (entriesFI newFile)
      -- New entries take precedence, but we keep the header from the base file
      mergedMap = Map.union newMap baseMap
      mergedEntries = map (\((ctx, key), val) -> LocEntry ctx key val) (Map.toList mergedMap)
  in LocFile (headerFI baseFile) mergedEntries

-- | Create a default header with UTF-8 content type
defaultHeader :: Text
defaultHeader = "Content-Type: text/plain; charset=utf-8\n"

-- | Create a new empty localization file with default header
emptyLocFile :: LocFile
emptyLocFile = LocFile defaultHeader []

-- | Fix common issues in a .po/.pot file
fixPoFile :: FilePath -> FilePath -> IO (Either (M.ParseErrorBundle Text Void) ())
fixPoFile inputPath outputPath = do
  result <- parseLocFile inputPath
  case result of
    Left err -> return $ Left err
    Right locFile -> do
      saveLocFile outputPath locFile
      return $ Right ()

-- | Validate a .po/.pot file without modifying it
validatePoFile :: FilePath -> IO (Either (M.ParseErrorBundle Text Void) ())
validatePoFile filePath = do
  result <- parseLocFile filePath
  return $ case result of
    Left err -> Left err
    Right _ -> Right ()

-- | Print a user-friendly error message to stderr
printParseError :: M.ParseErrorBundle Text Void -> IO ()
printParseError = hPutStrLn stderr . M.errorBundlePretty


fixLocale :: T.Text -> T.Text -> T.Text
fixLocale locale mName =
  let
    moduleComps = T.splitOn "/" mName
    moduleName = case reverse moduleComps of
      (_:p2:_) -> p2
      _ -> ""
  in
  case T.breakOn "." locale of
    (_, "") -> locale
    (aFileName, _) ->
      if aFileName == moduleName then
        "en"
      else
        aFileName
