module Generation.Elm where

import qualified Data.ByteString as Bs
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Parsing.Python as Py
import qualified Generation.Sql as Sq
import qualified Generation.Svg as Sv

data FunctionDef = FunctionDef {
  nameFD :: Text
  , typeDef :: TypeDef
  , argsFD :: [Text]
  , bodyFD :: ElmExpr
  }
  deriving (Show)

data TypeDef =
  StringTD
  | IntTD
  | FloatTD
  | BoolTD
  | RecordTD [(Text, TypeDef)]
  | MonadTD Text TypeDef
  | ApplicTD TypeDef TypeDef
  deriving (Show)


data ElmExpr =
  NameEE Text
  | LambdaEE [Text] ElmExpr
  | ApplyEE Text [ElmExpr]
  | LetEE [(Text, ElmExpr)] ElmExpr
  | IfEE ElmExpr ElmExpr ElmExpr
  | TupleEE [ElmExpr]
  | RecordEE [(Text, ElmExpr)]
  | CaseEE ElmExpr [(Text, [ElmExpr])]
  | ParentEE ElmExpr
  | LiteralEE ElmLiteral
  -- Specialized function application, for predefined Html functions.
  | HtEE ElementHt [AttributeHt] [ElmExpr]
  deriving (Show)

data ElmLiteral =
  StringL Text
  | IntL Int
  | FloatL Float
  deriving (Show)


data ElementHt =
  DivHE
  | TextHE
  | InputHE
  | ButtonHE
  | SelectHE
  | OptionHE
  | LabelHE
  | SpanHE
  | ImgHE
  | AHE
  | FormHE
  | TableHE
  | TdHE
  | ThHE
  | TrHE
  | TbodyHE
  | TheadHE
  | SectionHE
  | UlHE
  | LiHE
  | H1HE
  | H2HE
  | H3HE
  | H4HE
  | H5HE
  | H6HE
  | SvgHE Sv.SvgExpr
  | NavHE
  deriving (Show, Eq)


data AttributeHt =
  ClassHA ElmExpr
  | IdHA ElmExpr
  | ForHA ElmExpr
  | TypeHA ElmExpr
  | PlaceholderHA ElmExpr
  | RequiredHA Bool
  | AttributeHA ElmExpr ElmExpr
  | ValueHA ElmExpr
  | HrefHA ElmExpr
  | ScopeHA ElmExpr
  deriving (Show)

-- (HtEE DivHE) [HtEE ClassHA "container"] [(HtEE TextHE) "Hello"]
-- div [ class_ "container" ] [ text "Hello" ]

-- Elements:
div :: [AttributeHt] -> [ElmExpr] -> ElmExpr
div = HtEE DivHE

text :: Text -> ElmExpr
text aText = HtEE TextHE [] [LiteralEE (StringL aText)]

input :: [AttributeHt] -> [ElmExpr] -> ElmExpr
input = HtEE InputHE

button :: [AttributeHt] -> [ElmExpr] -> ElmExpr
button = HtEE ButtonHE

select :: [AttributeHt] -> [ElmExpr] -> ElmExpr
select = HtEE SelectHE

label :: [AttributeHt] -> [ElmExpr] -> ElmExpr
label = HtEE LabelHE

span :: [AttributeHt] -> [ElmExpr] -> ElmExpr
span = HtEE SpanHE

img :: [AttributeHt] -> [ElmExpr] -> ElmExpr
img = HtEE ImgHE

a :: [AttributeHt] -> [ElmExpr] -> ElmExpr
a = HtEE AHE

form :: [AttributeHt] -> [ElmExpr] -> ElmExpr
form = HtEE FormHE

table :: [AttributeHt] -> [ElmExpr] -> ElmExpr
table = HtEE TableHE

td :: [AttributeHt] -> [ElmExpr] -> ElmExpr
td = HtEE TdHE

th :: [AttributeHt] -> [ElmExpr] -> ElmExpr
th = HtEE ThHE

tr :: [AttributeHt] -> [ElmExpr] -> ElmExpr
tr = HtEE TrHE

tbody :: [AttributeHt] -> [ElmExpr] -> ElmExpr
tbody = HtEE TbodyHE

thead :: [AttributeHt] -> [ElmExpr] -> ElmExpr
thead = HtEE TheadHE

section :: [AttributeHt] -> [ElmExpr] -> ElmExpr
section = HtEE SectionHE

ul :: [AttributeHt] -> [ElmExpr] -> ElmExpr
ul = HtEE UlHE

li :: [AttributeHt] -> [ElmExpr] -> ElmExpr
li = HtEE LiHE

h1 :: [AttributeHt] -> [ElmExpr] -> ElmExpr
h1 = HtEE H1HE

h2 :: [AttributeHt] -> [ElmExpr] -> ElmExpr
h2 = HtEE H2HE

h3 :: [AttributeHt] -> [ElmExpr] -> ElmExpr
h3 = HtEE H3HE

h4 :: [AttributeHt] -> [ElmExpr] -> ElmExpr
h4 = HtEE H4HE

h5 :: [AttributeHt] -> [ElmExpr] -> ElmExpr
h5 = HtEE H5HE

h6 :: [AttributeHt] -> [ElmExpr] -> ElmExpr
h6 = HtEE H6HE

svg :: [Sv.AttributeSvg] -> [Sv.SvgExpr] -> ElmExpr
svg attribs children = HtEE (SvgHE (Sv.SvgSV attribs children)) [] []

nav :: [AttributeHt] -> [ElmExpr] -> ElmExpr
nav = HtEE NavHE

-- Attributes:
class_ :: Text -> AttributeHt
class_ aText = ClassHA (LiteralEE (StringL aText))

id :: Text -> AttributeHt
id aText = IdHA (LiteralEE (StringL aText))

for_ :: Text -> AttributeHt
for_ aText = ForHA (LiteralEE (StringL aText))

type_ :: Text -> AttributeHt
type_ aText = TypeHA (LiteralEE (StringL aText))

placeholder :: Text -> AttributeHt
placeholder aText = PlaceholderHA (LiteralEE (StringL aText))

required :: Bool -> AttributeHt
required = RequiredHA

attribute :: Text -> Text -> AttributeHt
attribute aName aValue = AttributeHA (LiteralEE (StringL aName)) (LiteralEE (StringL aValue))

value :: Text -> AttributeHt
value aText = ValueHA (LiteralEE (StringL aText))

href :: Text -> AttributeHt
href aText = HrefHA (LiteralEE (StringL aText))

scope :: Text -> AttributeHt
scope aText = ScopeHA (LiteralEE (StringL aText))

-- Show a function definition:
spitFct :: FunctionDef -> Bs.ByteString
spitFct aFct = T.encodeUtf8 aFct.nameFD <> " =\n" <> tabBy 1 (spitElm 1 aFct.bodyFD)


-- Generate text from an ElmExpr:
spitElm :: Int -> ElmExpr -> Bs.ByteString
spitElm level (NameEE aName) = T.encodeUtf8 aName
spitElm level (LambdaEE varNames aExpr) = "(\\" <> Bs.intercalate " " (map T.encodeUtf8 varNames) <> " ->\n" <> tabBy (level + 1) (spitElm (level + 1) aExpr) <> "\n)"
spitElm level (ApplyEE aName aExprs) = T.encodeUtf8 aName <> (if null aExprs then "" else " " <> Bs.intercalate " " (map (spitElm (level + 1)) aExprs))
spitElm level (LetEE aBindings aExpr) = "let\n" <> Bs.intercalate "\n" (map (tabBy (level + 1) . spitBinding (level + 1)) aBindings) <> "\n" <> tabBy level "in\n" <> tabBy level (spitElm (level + 1) aExpr)
spitElm level (IfEE testExpr trueExpr falseExpr) = "if " <> spitElm level testExpr <> " then\n" <> tabBy (level + 1) (spitElm level trueExpr) <> "\n" <> tabBy level "else\n" <> tabBy (level + 1) (spitElm level falseExpr)
spitElm level (TupleEE aExprs) = "(" <> Bs.intercalate ", " (map (spitElm level) aExprs) <> ")"
spitElm level (RecordEE aFields) = "{ " <> Bs.intercalate ", " (map (spitField level) aFields) <> " }"
spitElm level (CaseEE aExpr aCases) = "case " <> spitElm level aExpr <> " of\n" <> Bs.intercalate "\n" (map (tabBy (level + 1) . spitCase (level + 1)) aCases)
spitElm level (ParentEE aExpr) = "(" <> spitElm level aExpr <> ")"
spitElm level (LiteralEE aLiteral) = spitLiteral aLiteral
spitElm level (HtEE aHt aAttributes aChildren)
  | aHt == TextHE = "text " <> spitElm (level + 1) (head aChildren)
  | SvgHE svgExpr <- aHt = Sv.spitSvExpr (level + 1) svgExpr
  | otherwise =
    let
      aHtStr = case aHt of
        DivHE -> "div"
        TextHE -> "text"
        InputHE -> "input"
        ButtonHE -> "button"
        SelectHE -> "select"
        OptionHE -> "option"
        LabelHE -> "label"
        SpanHE -> "span"
        ImgHE -> "img"
        AHE -> "a"
        FormHE -> "H.form"
        TableHE -> "table"
        TdHE -> "td"
        ThHE -> "th"
        TrHE -> "tr"
        TbodyHE -> "tbody"
        TheadHE -> "thead"
        SectionHE -> "section"
        UlHE -> "ul"
        LiHE -> "li"
        H1HE -> "h1"
        H2HE -> "h2"
        H3HE -> "h3"
        H4HE -> "h4"
        H5HE -> "h5"
        H6HE -> "h6"
        NavHE -> "nav"
        -- If this is a 'pattern match redundant' warning, then all elements in the Sum type are covered:
        _ -> error "Unknown element"
    in
    aHtStr <> " [" <> Bs.intercalate ", " (map spitAttribute aAttributes) <> "] [" <> Bs.intercalate ", " (map (spitElm (level + 1)) aChildren) <> "]"


spitBinding :: Int -> (Text, ElmExpr) -> Bs.ByteString
spitBinding level (aName, aExpr) = T.encodeUtf8 aName <> " = " <> spitElm level aExpr

spitField :: Int -> (Text, ElmExpr) -> Bs.ByteString
spitField level (aName, aExpr) = T.encodeUtf8 aName <> " = " <> spitElm level aExpr

spitCase :: Int -> (Text, [ElmExpr]) -> Bs.ByteString
spitCase level (aName, aExprs) = T.encodeUtf8 aName <> " -> " <> Bs.intercalate " " (map (spitElm (level + 1)) aExprs)

spitLiteral :: ElmLiteral -> Bs.ByteString
spitLiteral (StringL aString) = "\"" <> T.encodeUtf8 aString <> "\""
spitLiteral (IntL aInt) = T.encodeUtf8 (T.pack (show aInt))
spitLiteral (FloatL aFloat) = T.encodeUtf8 (T.pack (show aFloat))

spitAttribute :: AttributeHt -> Bs.ByteString
spitAttribute attrib =
  case attrib of
    AttributeHA aName aValue -> "attribute " <> spitElm 0 aName <> " " <> spitElm 0 aValue
    ClassHA aClass -> "class " <> spitElm 0 aClass
    IdHA aId -> "id " <> spitElm 0 aId
    ForHA aFor -> "for " <> spitElm 0 aFor
    TypeHA aType -> "type_ " <> spitElm 0 aType
    PlaceholderHA aPlaceholder -> "placeholder " <> spitElm 0 aPlaceholder
    RequiredHA aRequired -> "required " <> (if aRequired then "True" else "False")
    ValueHA aValue -> "value " <> spitElm 0 aValue
    HrefHA aHref -> "href " <> spitElm 0 aHref
    ScopeHA aScope -> "scope " <> spitElm 0 aScope
    -- If this is 'pattern match redundant' warning, all attributes are covered.
    _ -> ""


tabBy :: Int -> Bs.ByteString -> Bs.ByteString
tabBy level = (<>) (Bs.replicate (2 * level) 32)