module Parsing.Pot where
import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 hiding (takeWhile, skipWhile)
import qualified Data.Text.Encoding as TE
import Data.Text(Text)
import Data.Word
import Data.ByteString (unsnoc, ByteString)
import qualified Data.ByteString.Char8 as B
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Either
import qualified Data.Text as T

data PORecord = PORecord {
        poComment :: Text,
        poMsgid :: Text,
        poMsgstr :: Text
    } deriving (Show, Eq)

takeTillEOL :: Parser ByteString
takeTillEOL = takeWhile (not . isEndOfLine)

parseMsgidMsgstrLine :: ByteString -> Parser Text
parseMsgidMsgstrLine key = do
  void (string key) <?> "Line key"
  skipSpace
  char '"' <?> "Opening Qutotation mark"
  val <- takeTillEOL
  endOfLine <?> "EOL"
  return $ TE.decodeUtf8 $ fromMaybe "" $ (fst <$> unsnoc val)

msgidLine = parseMsgidMsgstrLine "msgid"
msgstrLine = parseMsgidMsgstrLine "msgstr"

escapedTextLine :: Parser Text
escapedTextLine = char '"' *> (TE.decodeUtf8 <$> takeTillEOL) <* endOfLine
--escapedTextLine = do
--    char '"'
--    val <- takeTillEOL
--    return $ (traceShow val ())

nameP :: String -> Parser a -> Parser a
nameP str p = p <?> str

commentLine :: Parser Text
commentLine = nameP "comment line" $ do
    char '#' <?> "Line start hash"
    -- Skip space but not newline
    void $ many (char ' ')
    txt <- TE.decodeUtf8 <$> takeTillEOL
    endOfLine <?> "EOF"
    return txt

emptyLine :: Parser ()
emptyLine = skipSpace <* endOfLine

poRecord :: Parser PORecord
poRecord = do
    comments <- many1 commentLine <?> "Comments"
    msgidPrimary <- msgidLine <?> "msgid"
    extraMsgid <- many escapedTextLine <?> "Extra msgid"
    msgstrPrimary <- msgstrLine <?> "msgstr"
    extraMsgstr <- many escapedTextLine <?> "Extra msgstr"
    endOfLine
    let comment = T.intercalate "\n" comments
    let msgid = T.intercalate "\n" $ msgidPrimary : extraMsgid
    let msgstr = T.intercalate "\n" $ msgstrPrimary : extraMsgstr
    return $ PORecord comment msgid msgstr

poFile :: Parser [PORecord]
poFile =
    --let options = choice [emptyLine *> pure Nothing, ]
    many1 poRecord <?> "PO results"
