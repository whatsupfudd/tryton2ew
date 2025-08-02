module Parsing.Pot where

import Control.Applicative ((<|>))
import Control.Monad (void)

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Internal as Bi
import Data.Functor (($>))
import qualified Data.List as L
import qualified Data.List.NonEmpty as Ne
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Data.Word (Word8)

import System.IO (hPutStrLn, stderr)
import System.FilePath (takeFileName)

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as C
import qualified Text.Megaparsec.Byte.Lexer as CL


type Parser = M.Parsec Void Bs.ByteString

-- LocaleDefs : <locale> -> ( <module> -> [LocEntry] )
type LocaleForModule = Map.Map Bs.ByteString (Map.Map Bs.ByteString [LocEntry])

-- | Represents a single localization entry in the .po/.pot file
data LocEntry = LocEntry {
    contextEN :: !Bs.ByteString  -- ^ msgctxt: the <kind>:<v1>:[...], with <v1> = <model-name>,<field-name> or similar.
  , keyEN     :: !Bs.ByteString  -- ^ msgid: usually the content of the translation
  , valueEN   :: !Bs.ByteString  -- ^ msgstr: usually nothing.
  } deriving (Show, Eq)

-- | Represents a complete localization file with header and entries
data LocFile = LocFile {
    headerFI  :: !Bs.ByteString      -- ^ The header content (usually content type info)
  , entriesFI :: ![LocEntry] -- ^ The list of localization entries
  } deriving (Show, Eq)

-- | Parses a complete .po/.pot file with detailed error reporting
parseLocFile :: FilePath -> IO (Either (M.ParseErrorBundle Bs.ByteString Void) LocFile)
parseLocFile filePath = do
  content <- Bs.readFile filePath
  pure $ M.runParser locFileParser filePath content


parseLocFileWithDiagnostics :: FilePath -> IO (Either (M.ParseErrorBundle Bs.ByteString Void) LocFile)
parseLocFileWithDiagnostics filePath = do
  result <- parseLocFile filePath
  case result of
    Right locFile -> pure $ Right locFile
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
      pure $ Left err


-- | Parse a string directly (useful for testing)
parseLocString :: String -> Bs.ByteString -> Either (M.ParseErrorBundle Bs.ByteString Void) LocFile
parseLocString = M.runParser locFileParser

-- | Parser for an entire localization file
locFileParser :: Parser LocFile
locFileParser = do
  sc
  header <- M.option "" headerParser  -- Optional header
  entries <- M.many entryParser
  M.eof
  return $ LocFile header entries

-- | Parser for the file header
headerParser :: Parser Bs.ByteString
headerParser = M.label "file header" $ do
  -- First, check for a header
  hasHeader <- M.option False (C.char (Bi.c2w '#') $> True)
  
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
contextParser :: Parser Bs.ByteString
contextParser = M.label "msgctxt field" $ do
  C.string "msgctxt" *> sc
  quotedStringParser

-- | Parser for msgid field
keyParser :: Parser Bs.ByteString
keyParser = M.label "msgid field" $ do
  C.string "msgid" *> sc
  quotedStringParser

-- | Parser for msgstr field
valueParser :: Parser Bs.ByteString
valueParser = M.label "msgstr field" $ do
  C.string "msgstr" *> sc
  quotedStringParser

-- | Parser for quoted strings, which can span multiple lines
quotedStringParser :: Parser Bs.ByteString
quotedStringParser = M.label "quoted string" $ do
  strings <- M.some quotedPart
  pure $ Bs.concat strings
  where
    quotedPart = do
      C.char (Bi.c2w '"')
      -- content <- M.try escapedStringContent <|> simpleStringContent
      content <- parseStringContent
      C.char (Bi.c2w '"') M.<?> "closing quote (found unclosed string, possibly unescaped backslash before this point)"
      sc
      pure (Bs.pack content)


    parseStringContent =
      -- Build the string character by character, handling escapes
      M.many parseChar
    

    -- Parse a single character, handling escapes
    parseChar :: Parser Word8
    parseChar = M.try parseEscaped <|> parseNormal
    
    -- Parse an escaped character sequence
    parseEscaped :: Parser Word8
    parseEscaped = do
      C.char (Bi.c2w '\\')  -- A single backslash as escape character
      M.satisfy (\c -> c == Bi.c2w '"' || c == Bi.c2w '\\' || c == Bi.c2w 'n' || c == Bi.c2w 'r' || c == Bi.c2w 't') M.<?> "valid escape character (\\, \", n, r, or t)"
      {- pure $ case c of
        '\\' -> Bi.c2w '\\'  -- Backslash
        '"'  -> Bi.c2w '"'   -- Quote
        'n'  -> Bi.c2w '\n'  -- Newline
        'r'  -> Bi.c2w '\r'  -- Carriage return
        't'  -> Bi.c2w '\t'  -- Tab
        _    -> Bi.c2w c     -- Should not happen due to the satisfy
      -}

    -- Parse a non-escaped character
    parseNormal :: Parser Word8
    parseNormal = M.noneOf [Bi.c2w '\\', Bi.c2w '"']

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
  C.char (Bi.c2w '#')
  M.skipMany (M.satisfy (\c -> c /= Bi.c2w '\n'))
  void C.eol <|> M.eof
  pure ()

-- | Space consumer that handles whitespace and typical comments
-- but preserves the special header comment
sc :: Parser ()
sc = do
  scNoComments
  M.skipMany (commentLine *> scNoComments)

-- | Convert entries to a Map for efficient lookups
entriesToMap :: [LocEntry] -> Map (Bs.ByteString, Bs.ByteString) Bs.ByteString
entriesToMap = Map.fromList . map (\e -> ((e.contextEN, e.keyEN), e.valueEN))

-- | Look up a translation by context and key
lookupTranslation :: LocFile -> Bs.ByteString -> Bs.ByteString -> Maybe Bs.ByteString
lookupTranslation locFile ctx key = 
  Map.lookup (ctx, key) (entriesToMap locFile.entriesFI)


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
formatHeader :: Bs.ByteString -> Bs.ByteString
formatHeader headerContent =
  Bs.intercalate "\n"
    [ "#"
    , "msgid \"\""
    , "msgstr " <> (if Bs.null headerContent then "\"\"" else headerContent)
    ]

-- | Format a localization entry back to the .po/.pot format
formatEntry :: LocEntry -> Bs.ByteString
formatEntry entry = Bs.intercalate "\n"
  [ "msgctxt " <> formatString entry.contextEN
  , "msgid " <> formatString entry.keyEN
  , "msgstr " <> formatString entry.valueEN
  , ""  -- Empty line between entries
  ]
  where
    formatString :: Bs.ByteString -> Bs.ByteString
    formatString s
      | Bs.null s = "\"\""
      | Bs.length s > 60 = formatMultiline s
      | otherwise = "\"" <> s <> "\""
    
    formatMultiline :: Bs.ByteString -> Bs.ByteString
    formatMultiline s = Bs.intercalate "\n" $ map (\line -> "\"" <> line <> "\"") (splitTextLines s 60)
    
    splitTextLines :: Bs.ByteString -> Int -> [Bs.ByteString]
    splitTextLines txt maxLen
      | Bs.length txt <= maxLen = [txt]
      | otherwise = let (first, rest) = Bs.splitAt maxLen txt
                    in first : splitTextLines rest maxLen

-- | Save a localization file
saveLocFile :: FilePath -> LocFile -> IO ()
saveLocFile filePath locFile = do
  let headerText = formatHeader locFile.headerFI
      entriesText = Bs.concat $ map formatEntry locFile.entriesFI
      content = headerText <> entriesText
  Bs.writeFile filePath content

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
defaultHeader :: Bs.ByteString
defaultHeader = "Content-Type: text/plain; charset=utf-8\n"

-- | Create a new empty localization file with default header
emptyLocFile :: LocFile
emptyLocFile = LocFile defaultHeader []

-- | Fix common issues in a .po/.pot file
fixPoFile :: FilePath -> FilePath -> IO (Either (M.ParseErrorBundle Bs.ByteString Void) ())
fixPoFile inputPath outputPath = do
  result <- parseLocFile inputPath
  case result of
    Left err -> return $ Left err
    Right locFile -> do
      saveLocFile outputPath locFile
      return $ Right ()

-- | Validate a .po/.pot file without modifying it
validatePoFile :: FilePath -> IO (Either (M.ParseErrorBundle Bs.ByteString Void) ())
validatePoFile filePath = do
  result <- parseLocFile filePath
  return $ case result of
    Left err -> Left err
    Right _ -> Right ()

-- | Print a user-friendly error message to stderr
printParseError :: M.ParseErrorBundle Bs.ByteString Void -> IO ()
printParseError = hPutStrLn stderr . M.errorBundlePretty


fixLocale :: Bs.ByteString -> Bs.ByteString -> Bs.ByteString
fixLocale locale mName =
  let
    moduleComps = Bs.split 47 mName  -- 47 -> /
    moduleName = case reverse moduleComps of
      (_:p2:_) -> p2
      _ -> ""
  in
  case Bs.break (== 46) locale of   -- 46 -> .
    (_, "") -> locale
    (aFileName, _) ->
      if aFileName == moduleName then
        "en"
      else
        aFileName
