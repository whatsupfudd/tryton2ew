{-# LANGUAGE QuasiQuotes #-}

module Generation.Fuddle where

import qualified Data.ByteString as Bs
import qualified Data.Map.Strict as Mp
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Text.RawString.QQ (r)

import qualified Tryton.Types as Tm
import qualified Generation.Elm as E
import qualified Generation.Views as Vw
import Generation.EwTypes
import Data.Maybe (fromMaybe)


leftPartAItems :: [Menu] -> Bs.ByteString
leftPartAItems menus =
  let
    templ_1 = [r|module Protected.LeftMenuNav exposing (leftPartAItems)

import Html.String exposing (..)
import Html.String as H
import Html.String.Attributes exposing (..)

import Protected.LeftMenuDef exposing (ItemLA (..))

leftPartAItems : List (ItemLA msg)
leftPartAItems = [|]
  in
  templ_1
    <> "\n" <> genMenus 1 menus
    <> "\n  ]"


genMenus :: Int -> [Menu] -> Bs.ByteString
genMenus level =
  let
    indent = Bs.replicate (level * 2) 32
  in
  foldl (\accum aMenu ->
      let
        menuEntry = genLeftMenuItem level aMenu
      in
      case accum of
        "" -> indent <> menuEntry
        _ -> accum <> "\n" <> indent <> ", " <> menuEntry
    ) ""



genLeftMenuItem :: Int -> Menu -> Bs.ByteString
genLeftMenuItem level aMenu =
  let
    indent = Bs.replicate (level * 2) 32
  in
  if null aMenu.children then
    indent <> "Simple {\n    title = \"" <> T.encodeUtf8 aMenu.label
    <> "\"\n" <> indent <> "    , href = \"" <> T.encodeUtf8 aMenu.mid
    <> "\"\n" <> indent <> "    , mid = \"" <> T.encodeUtf8 aMenu.mid
    <> "\"\n" <> indent <> "    , icon = " <> maybe "Nothing" (\i -> "Just (img [ src \"" <> T.encodeUtf8 i <> "\", width 32 ] [])") aMenu.icon
    <> "\n" <> indent <> "    , postIcon = Nothing"
    <> "\n" <> indent <> "    , params = Nothing"
    <> "\n" <> indent <> "  }"
  else
    indent <> "Composed {\n" <> indent <> "   uid = \"" <> T.encodeUtf8 (T.toLower aMenu.label)
    <> "\"\n" <> indent <> "    , icon = " <> maybe "Nothing" (\i -> "Just (img [ src \"" <> T.encodeUtf8 i <> "\", width 32 ] [])") aMenu.icon
    <> "\n" <> indent <> "    , title = \"" <> T.encodeUtf8 aMenu.label
    <> "\"\n" <> indent <> "    , children = " <> case genMenus (level + 1) aMenu.children of
      "" -> "[]"
      children -> "[\n" <> children <> "\n    ]"
    <> "\n" <> indent <> "  }"


genDynRoutes :: [Component] -> Bs.ByteString
genDynRoutes components =
  let
    templ_1 = [r|module DynRoutes exposing (main)
import Dict as D
import Tuple as T

import FunctionRouter exposing (run)

-- Components:|]

    templ_2 = [r|

-- This must be created on a per-instance of app/UI:
-- uiFunctions : D.Dict String (ComponentBuilder msg pT)
dynSpecifications = [
  |]

    templ_3 = [r|
  ]

-- This should be part of the FunctionRouter library:
main =
  let
    dynFunctions = D.fromList <| List.map (\(label, fct,_) -> (label, fct)) dynSpecifications
    compContinuations = D.fromList <| List.foldl (
        \(label, _, conts) accum -> accum ++ (
            List.map (\(contLabel, handler) -> (label ++ "." ++ contLabel, handler)) conts)
      ) [] dynSpecifications
  in
    run (dynFunctions, compContinuations)|]
  in
  templ_1 <> "\n"
   <> Bs.intercalate "\n" (map (\aComp -> let modName = T.encodeUtf8 aComp.moduleName in "import Components." <> modName <> " as " <> modName) components)
   <> templ_2
   <> Bs.intercalate "\n  , " (map (\aComp -> let modName = T.encodeUtf8 aComp.moduleName in "(\"" <> T.encodeUtf8 aComp.refID <> "\", " <> modName <> ".default, " <> modName <> ".continuations)") components)
   <> templ_3


renderComponent :: Component -> Bs.ByteString
renderComponent component =
  let
    templ_1 = [r| exposing (default, continuations)

import Dict as D
import Tuple as T

import Html.String exposing (..)
import Html.String as H
import Html.String.Attributes exposing (..)
import Sup.Svg as S
import Sup.Attributes as Sa
import Html.Htmx as Ht
import Html.Extra as Hx
import Html.Flowbite as Fb

import Json.Encode as Enc
import Json.Decode as Dec

import FunctionRouter exposing (DynInvokeFct, CompContinuation, InvokeResult (..), NativeParams)

-- import Fuddle.Routes as R
import Fuddle.Locales exposing (l)

resultParser : Dec.Decoder a -> Dec.Decoder a
resultParser rezParser =
  Dec.field "result" rezParser


type alias FetchArgs = {
    offset : Int
    , limit : String
  }


fetchParser : Dec.Decoder FetchArgs
fetchParser =
  Dec.map2 FetchArgs
    (Dec.field "offset" Dec.int)
    (Dec.field "limit" Dec.string)

hsContName_1 = "|]

  -- Add component name.
    templ_2 = [r|_showBodyCt"
hsFetchRows : FetchArgs -> NativeParams
hsFetchRows fetchArgs = {
    package = "gnuhealth.dbops"
    , action = "|]

  -- Add hsForward function name.
    templ_3 = [r|"
    , rcpt = hsContName_1
    , params = Enc.object [ ("offset", Enc.int fetchArgs.offset), ("limit", Enc.int (limitNameToInt fetchArgs.limit)) ]
  }

limitNameToInt : String -> Int
limitNameToInt limitName =
  case limitName of
    "s1" -> 15
    "s2" -> 25
    "s3" -> 50
    "s4" -> 100
    _ -> 25


continuations : List (String, CompContinuation msgT)
continuations = [(hsContName_1, showBodyCt)]


default : DynInvokeFct msgT
default _ jsonParams =
  Forward [ demoFct ]


|]

    -- Add the component name.
    templ_4 = [r|_fetch jsonParams=
  case Dec.decodeValue fetchParser jsonParams of
    Err err -> Forward <| [
        div
          [ class "text-red" ]
          [ text <| "@[|]

    -- Add the component name.
    templ_5 = [r|_fetch]: error in params: " ++ Dec.errorToString err ]
      ]
    Ok localArgs ->
      ExecNative <| hsFetchRows localArgs

-- TODO: implement the real record type:
type alias HsRow = {|]
    -- Add the fields of the HsRow record.
    templ_6 = [r|}


bodyParser : Dec.Decoder (List HsRow)
bodyParser  =
  Dec.list hsRowParser


hsRowParser : Dec.Decoder HsRow
hsRowParser =
  |]

    -- Add the parser for all the fields of a hsForward result:
    templ_7 = [r|

showBodyCt : CompContinuation msgT
showBodyCt _ jsonParams =
  case Dec.decodeValue (resultParser bodyParser) jsonParams of
    Err err -> Forward [
        div
          [ class "text-red" ]
          [ text <| "@[|]

    -- Add component name.
    templ_8 = [r|.showBodyCt]: error in params: " ++ Dec.errorToString err ]
      ]
    Ok tRows -> Forward <| showTableRows tRows

-- TODO: implement the render of the tbody rows:
showTableRows someRows = []

|]
    componentName = T.encodeUtf8 $ T.toLower component.moduleName
    hsForwardFct = "hs_" <> componentName <> "_fetch"
    -- TODO: do a real encoding of the View fields:
    fieldDefs = [("field_1", "String"), ("field_2", "Int")]
  in
  "module Components." <> T.encodeUtf8 component.moduleName
  <> Bs.concat [templ_1, hsForwardFct, templ_2
        , componentName, templ_3
        , componentName, templ_4
        , componentName, templ_5
        , buildHsFields fieldDefs, templ_6
        , buildHsRowFields fieldDefs, templ_7
        , componentName, templ_8
      ]
  <> Bs.intercalate "\n\n" (map E.spitFct component.functions)
 -- <> T.encodeUtf8 component.moduleName <> "\" ]]\n"


buildHsFields :: [(Bs.ByteString, Bs.ByteString)] -> Bs.ByteString
buildHsFields fieldDefs = Bs.intercalate "\n    , " [ fst aField <> " : " <> snd aField | aField <- fieldDefs ]


buildHsRowFields :: [(Bs.ByteString, Bs.ByteString)] -> Bs.ByteString
buildHsRowFields fieldDefs =
  "Dec.map" <> (T.encodeUtf8 . T.pack . show) (length fieldDefs)
  <> " HsRow\n    "
  <> Bs.intercalate "\n    " (map fieldDecoder fieldDefs)


fieldDecoder :: (Bs.ByteString, Bs.ByteString) -> Bs.ByteString
fieldDecoder (fieldName, fieldType) = "(Dec.field \"" <> fieldName <> "\" " <> typeDecoder fieldType <> ")"


typeDecoder :: Bs.ByteString -> Bs.ByteString
typeDecoder fieldType = case fieldType of
  "String" -> "Dec.string"
  "Int" -> "Dec.int"
  _ -> "Dec.string"


genFunction :: Mp.Map Bs.ByteString CompLocales -> T.Text -> Maybe ActionWindow -> [E.FunctionDef]
genFunction locales refID mbActionWin =
  let
    mbDefaultLocales = Mp.lookup "en" locales
  in
  case mbActionWin of
    Nothing ->
      [
        E.FunctionDef {
          nameFD = "demoFct"
          , argsFD = []
          , typeDef = E.StringTD
          , events = []
          , bodyFD =
              E.section [ E.class_ "bg-gray-50 dark:bg-gray-900 p-3 sm:p-5 md:ml-64 lg:mr-16 min-h-full pt-20" ] [
                E.div [E.class_ "text-red-500" ] [ E.text "No action window." ]
              ]
        }
      ]
    Just actionWin ->
      let
        subFcts = foldl (\accum (vName, viewDef) ->
            case viewDef of
              Tm.TreeDF attribs elements ->
                Vw.genTree vName elements : accum
              Tm.FormDF {} -> accum
          ) [] (Mp.toList actionWin.uiViewsAW)
        xlatedLogicName = T.decodeUtf8 $ translateModelName mbDefaultLocales actionWin.logicNameAW
      in
      [
        E.FunctionDef {
          nameFD = "demoFct"
          , argsFD = []
          , typeDef = E.MonadTD "H.Html" (E.VarTD "msg")
          , bodyFD = E.div [E.class_ "bg-gray-50 dark:bg-gray-900 p-4 md:ml-64 lg:mr-16 min-h-full pt-20" ] $ case subFcts of
              [] -> [ E.div [E.class_ "text-red-500" ] [
                        E.text xlatedLogicName
                      , E.text "No view nor form defined."
                    ]]
              _ -> [
                E.div [ E.class_ "mx-auto max-w-screen-xl px-4 lg:px-12" ]
                  ([ E.div [ E.class_ "flex justify-between items-center mb-4" ]
                    [ E.h1 [ E.class_ "text-2xl text-gray-900 dark:text-white font-bold" ] [ E.text xlatedLogicName ] ]
                  ]
                  <> [ E.ApplyEE (T.decodeUtf8 $ translateFieldName mbDefaultLocales actionWin.logicNameAW aFct.nameFD) [] | (aFct, _) <- subFcts ]
                  )
                ]
          , events = concat [ aFct.events <> bFct.events | (aFct, bFct) <- subFcts ]
        }
      ] <> concat [ [aFct, bFct] | (aFct, bFct) <- subFcts ]

{-
type ModelLocale = Mp.Map Bs.ByteString (Mp.Map Bs.ByteString (Mp.Map Bs.ByteString (Mp.Map Bs.ByteString Bs.ByteString)))
-}

translateModelName :: Maybe CompLocales -> T.Text -> Bs.ByteString
translateModelName mbLocales aName =
  let
    modelName = T.encodeUtf8 aName
  in
  case mbLocales of
    Nothing -> modelName
    Just locales -> fromMaybe modelName ( -- TODO: use the locale
        case Mp.lookup modelName locales.modelCL >>= Mp.lookup "name" >>= Mp.lookup ""  of
          Nothing -> Nothing
          Just cMap -> case Mp.keys cMap of
            [] -> Nothing
            (aKey : _) -> Just aKey
      )

translateFieldName :: Maybe CompLocales -> T.Text -> T.Text -> Bs.ByteString
translateFieldName mbLocales fstName sndName =
  let
    modelName = T.encodeUtf8 fstName
    fieldName = T.encodeUtf8 sndName
  in
  case mbLocales of
    Nothing -> fieldName
    Just locales -> fromMaybe fieldName (
        case Mp.lookup modelName locales.fieldCL >>= Mp.lookup fieldName of
          Nothing -> Nothing
          Just cMap -> case Mp.keys cMap of
            [] -> Nothing
            (aKey : _) -> Just aKey
      )