module Generation.Svg where

import qualified Data.ByteString as Bs
import Data.Text (Text)


data SvgExpr =
  SvgSV [AttributeSvg] [SvgExpr]
  | PathSV [AttributeSvg]
  deriving (Show, Eq)

data AttributeSvg =
  AttributeSv Bs.ByteString Bs.ByteString
  | ClassSv Bs.ByteString
  | FillSv Bs.ByteString
  | StrokeSv Bs.ByteString
  | StrokeWidthSv Bs.ByteString
  | ViewBoxSv Bs.ByteString
  | DSv Bs.ByteString
  | ClipRuleSv Bs.ByteString
  | FillRuleSv Bs.ByteString
  deriving (Show, Eq)

-- Elements:
path :: [AttributeSvg] -> [SvgExpr] -> SvgExpr
path attribs _ = PathSV attribs

-- Attributes:
attribute :: Bs.ByteString -> Bs.ByteString -> AttributeSvg
attribute = AttributeSv

class_ :: Bs.ByteString -> AttributeSvg
class_ = ClassSv

fill :: Bs.ByteString -> AttributeSvg
fill = FillSv

stroke :: Bs.ByteString -> AttributeSvg
stroke = StrokeSv

strokeWidth :: Bs.ByteString -> AttributeSvg
strokeWidth = StrokeWidthSv

viewBox :: Bs.ByteString -> AttributeSvg
viewBox = ViewBoxSv

d :: Bs.ByteString -> AttributeSvg
d = DSv

clipRule :: Bs.ByteString -> AttributeSvg
clipRule = ClipRuleSv

fillRule :: Bs.ByteString -> AttributeSvg
fillRule = FillRuleSv


-- Show an SVG expression:
spitSvExpr :: Int -> SvgExpr -> Bs.ByteString
spitSvExpr level (SvgSV attribs children) = "S.svg [" <> Bs.intercalate ", " (map spitAttribute attribs) <> "] [" <> Bs.intercalate ", " (map (spitSvExpr (level + 1)) children) <> "]"
spitSvExpr level (PathSV attribs) = "S.path [" <> Bs.intercalate ", " (map spitAttribute attribs) <> "] []"

spitAttribute :: AttributeSvg -> Bs.ByteString
spitAttribute attrib =
  case attrib of
    AttributeSv aName aValue -> "attribute \"" <> aName <> "\" \"" <> aValue <> "\""
    ClassSv aClass -> "Sa.class \"" <> aClass <> "\""
    FillSv aFill -> "Sa.fill \"" <> aFill <> "\""
    StrokeSv aStroke -> "Sa.stroke \"" <> aStroke <> "\""
    StrokeWidthSv aStrokeWidth -> "Sa.strokeWidth \"" <> aStrokeWidth <> "\""
    ViewBoxSv aViewBox -> "Sa.viewBox \"" <> aViewBox <> "\""
    DSv aD -> "Sa.d \"" <> aD <> "\""
    ClipRuleSv aClipRule -> "Sa.clipRule \"" <> aClipRule <> "\""
    FillRuleSv aFillRule -> "Sa.fillRule \"" <> aFillRule <> "\""
    -- If this is 'patter redundant' warning, all attributes are covered.
    _ -> ""
