{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant maybe" #-}
{-# HLINT ignore "Use <|>" #-}
{-# HLINT ignore "Use =<<" #-}
module Generation.EasyWordy where

import qualified Data.ByteString as Bs
import qualified Data.Char as C
import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as T
import Text.RawString.QQ (r)

import System.FilePath ((</>))

import qualified Parsing.Tryton as Tt
import qualified Parsing.Pot as Po
import Generation.EwTypes


type UiDefs = ([Tt.MenuItem], Mp.Map T.Text [Tt.ModelElement], Tt.ViewDefs)

{-
Need to generate:
  - wapp/DesktopNav.elm menu items.
  - wapp/Components/<xyz>.elm for each menu item.
  - Currently no wapp/BuildPage.elm entries, everything is dynamic
  - one entry per component wapp/DynRoutes.elm entries, currently everything is dynamic and each menu item has its own component file.
      dynSpecifications = [
        ("mb.showUserMailboxes", Mb.default, Mb.continuations)
      ]
  - wapp/Intl/<locale>.elm for each locale.
  - endpoint definitions in <app>.yaml, eg:
    - id: mb.showUserMailboxes
    action:
      Function:
        DynRoutes: mb.showUserMailboxes

  - SQL table definitions (.sql) for each SqlModel found in the .py files.
  - SQL manipulation functions in for EW internal API (.hs) for each ModelView found in the .py files.
-}


generateApp :: FilePath -> UiDefs -> Po.LocaleDefs -> [TableDef] -> IO (Either String ())
generateApp destPath uiDefs locales dbDefs =
  let
    consoLocales = consolidateLocales locales
    eiLeftMenuNav = genLeftMenu uiDefs consoLocales
  in
  case eiLeftMenuNav of
    Left err -> do
      pure $ Left err
    Right menus -> do
      let
        components = genComponents uiDefs menus consoLocales
        dynRoutes = genDynRoutes components
        yamlEntries = genFunctionDefs components
        context = EwContext {
          components = components
          , menus = menus
          , appEntries = []
        }
        renderedMenus = leftPartAItems menus
      TIO.writeFile (destPath </> "wapp/Protected/LeftMenuNav.elm") (T.decodeUtf8 renderedMenus)
      TIO.writeFile (destPath </> "wapp/DynRoutes.elm") (T.decodeUtf8 dynRoutes)
      TIO.writeFile (destPath </> "yamlEntries.txt") $ T.decodeUtf8 yamlEntries
      TIO.writeFile (destPath </> "compLocales.txt") $ T.pack (show consoLocales)
      mapM_ (saveComponent destPath) components
      pure $ Right ()


genLeftMenu :: UiDefs -> Mp.Map Bs.ByteString CompLocales -> Either String [Menu]
genLeftMenu (menuItems, modelDefs, _) locales =
  let
    enLocales = Mp.lookup "en" locales
    iconDefs = Mp.lookup "ir.ui.icon" modelDefs
    menus = analyseMenuItems enLocales iconDefs menuItems
  in
  Right menus


genComponents :: UiDefs -> [Menu] -> Mp.Map Bs.ByteString CompLocales -> [Component]
genComponents (menuItems, modelDefs, viewDefs) leftMenus locales =
  concatMap (\aMenu ->
      let
        componentName = convertMenuID aMenu.mid
        fileName = "wapp/Components/" <> T.unpack componentName <> ".elm"
        topComp = Component {
              path = fileName
            , moduleName = componentName
            , refID = aMenu.mid
            , types = []
            , functions = []
            , locales = Mp.empty  -- Fix this.
          }
        childrenComponents = genComponents (menuItems, modelDefs, viewDefs) aMenu.children locales
      in
      topComp : childrenComponents
    ) leftMenus
  

genSqlCode :: [Tt.Definition] -> IO (Either String ())
genSqlCode defs = pure $ Right ()


convertMenuID :: T.Text -> T.Text
convertMenuID oriName =
  let
    nameParts = concatMap (T.splitOn "_") (T.splitOn "." oriName)
  in
  T.intercalate "_" (map capitalize nameParts)

capitalize :: T.Text -> T.Text
capitalize aWord = T.cons (C.toUpper $ T.head aWord) (T.tail aWord)


saveComponent :: FilePath -> Component -> IO (Either String ())
saveComponent destPath component =
  let
    rendered = renderComponent component
  in do
  Bs.writeFile (destPath </> component.path) rendered
  pure $ Right ()


renderComponent :: Component -> Bs.ByteString
renderComponent component =
  let
    templ_1 = [r| exposing (default, continuations)

import Dict as D
import Tuple as T

import Html.String exposing (..)
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
{-
type alias LocalArgs = {
    userID : Int
}

argParser : Dec.Decoder LocalArgs
argParser =
  Dec.map LocalArgs (Dec.field "userID" Dec.int)

resultParser : Dec.Decoder a -> Dec.Decoder a
resultParser rezParser =
  Dec.field "result" rezParser
-}

continuations : List (String, CompContinuation msgT)
continuations = []

default : DynInvokeFct msgT
default _ jsonParams =
  {-
  case Dec.decodeValue argParser jsonParams of
    Err err -> Forward <| [
        div
          [ class "text-red" ]
          [ text <| "@[COMPONENT.default]: error in params: " ++ Dec.errorToString err ]
      ]
    Ok localArgs ->
      ExecNative <| CONTINUATION userInfo
  -}
  Forward [ demoFct ]

demoFct =
  div [ class "bg-gray-50 dark:bg-gray-900 p-4 md:ml-64 lg:mr-16 min-h-full pt-20" ] [ div [ class "text-red-500" ] [ text "|]
  in
  "module Components." <> T.encodeUtf8 component.moduleName
  <> templ_1
  <> T.encodeUtf8 component.moduleName <> "\" ]]\n"

consolidateLocales :: Po.LocaleDefs -> Mp.Map Bs.ByteString CompLocales
consolidateLocales =
  Mp.foldlWithKey consoLocale Mp.empty
  where
  consoLocale :: Mp.Map Bs.ByteString CompLocales -> Bs.ByteString -> Mp.Map Bs.ByteString [Po.LocEntry] -> Mp.Map Bs.ByteString CompLocales
  consoLocale accum aLocale modContent =
      let
        compLoc = fromMaybe defaultCompLocales (Mp.lookup aLocale accum)
        updLoc = foldl consoKind compLoc (Mp.toList modContent)
      in
      Mp.insert aLocale updLoc accum
  consoKind :: CompLocales -> (Bs.ByteString, [Po.LocEntry]) -> CompLocales
  consoKind compLoc (aModule, locEntries) =
    foldl (\accum aLocEntry ->
        let
          (kind, content) = locEntryKind aLocEntry
        in
        case kind of
          "model" -> accum { modelCL = updLocModel accum.modelCL content aLocEntry }
          "field" -> accum { fieldCL = updLocField accum.fieldCL content aLocEntry }
          "help" -> accum { helpCL = updLocHelp accum.helpCL content aLocEntry }
          "selection" -> accum { selectionCL = updLocSelection accum.selectionCL content aLocEntry }
          "view" -> accum { viewCL = updLocView accum.viewCL content aLocEntry }
          "wizardButton" -> accum { wizardButtonCL = updLocWizardButton accum.wizardButtonCL content aLocEntry }
          _ -> accum { errors = "unknown kind: " <> kind : accum.errors }
      ) compLoc locEntries

  locEntryKind :: Po.LocEntry -> (Bs.ByteString, Bs.ByteString)
  locEntryKind aLocEntry =
    let
      (someKind, rest) = Bs.break (== 58) aLocEntry.contextEN  -- 58 -> :
    in
    case rest of
      "" -> ("error: no content", "")
      _ ->
        if someKind `elem` ["model", "field", "help", "selection", "view", "wizardButton"] then
          (someKind, Bs.tail rest)
        else
          ("error: unknown kind: " <> someKind, "")

  updLocModel :: ModelLocale -> Bs.ByteString -> Po.LocEntry -> ModelLocale
  updLocModel accum content aLocEntry =
    case Bs.break (== 44) content of     -- 44 -> ,
      (_, "") -> accum
      (modelName, rest) ->
        let
          modelLoc = fromMaybe Mp.empty (Mp.lookup modelName accum)
        in
        case Bs.break (== 58) (Bs.tail rest) of     -- 58 -> :
          (_, "") -> accum -- no <key>:<value> pair (eg "name:gaga"), so ignore.
          (modKey, allModValue) ->
            let
              modValue = Bs.tail allModValue
            in
            case Mp.lookup modKey modelLoc of
              Nothing ->
                let
                  updModelLoc = Mp.singleton modKey (Mp.singleton modValue (Mp.singleton aLocEntry.keyEN aLocEntry.valueEN))
                in
                Mp.insert modelName updModelLoc accum
              Just keyMap ->
                case Mp.lookup modValue keyMap of
                  Nothing ->
                    let
                      updKeyMap = Mp.insert modValue (Mp.singleton aLocEntry.keyEN aLocEntry.valueEN) keyMap
                      updModelLoc = Mp.insert modKey updKeyMap modelLoc
                    in
                    Mp.insert modelName updModelLoc accum
                  Just locMap ->
                    let
                      updLocMap = Mp.insert aLocEntry.keyEN aLocEntry.valueEN locMap
                      updKeyMap = Mp.insert modValue updLocMap keyMap
                      updModLoc = Mp.insert modKey updKeyMap modelLoc
                    in
                    Mp.insert modelName updModLoc accum

  updLocField :: Mp.Map Bs.ByteString (Mp.Map Bs.ByteString Locales) -> Bs.ByteString -> Po.LocEntry -> Mp.Map Bs.ByteString (Mp.Map Bs.ByteString Locales)
  updLocField accum content aLocEntry =
    case Bs.break (== 44) content of     -- 44 -> ,
      (_, "") -> accum
      (modelID, allFieldID) ->
        let
          fieldID = Bs.tail allFieldID
          modelMap = fromMaybe Mp.empty (Mp.lookup modelID accum)
        in
        case Bs.break (== 58) fieldID of     -- 58 -> :
          (_, "") -> accum -- no fieldID, so ignore.
          (modKey, _) ->
            case Mp.lookup modKey modelMap of
              Nothing ->
                let
                  newDictEntry = Mp.singleton aLocEntry.keyEN aLocEntry.valueEN
                  newKeyMap = Mp.singleton modKey newDictEntry
                in
                Mp.insert modelID newKeyMap accum
              Just dictMap ->
                let
                  newDictEntry = Mp.insert aLocEntry.keyEN aLocEntry.valueEN dictMap
                  newKeyMap = Mp.insert modKey newDictEntry modelMap
                in
                Mp.insert modelID newKeyMap accum

  updLocHelp :: Mp.Map Bs.ByteString Locales -> Bs.ByteString -> Po.LocEntry -> Mp.Map Bs.ByteString Locales
  updLocHelp accum content aLocEntry = accum

  updLocSelection :: Mp.Map Bs.ByteString (Mp.Map Bs.ByteString Locales) -> Bs.ByteString -> Po.LocEntry -> Mp.Map Bs.ByteString (Mp.Map Bs.ByteString Locales)
  updLocSelection accum content aLocEntry = accum

  updLocView :: Mp.Map Bs.ByteString Locales -> Bs.ByteString -> Po.LocEntry -> Mp.Map Bs.ByteString Locales
  updLocView accum content aLocEntry = accum

  updLocWizardButton :: Mp.Map Bs.ByteString Locales -> Bs.ByteString -> Po.LocEntry -> Mp.Map Bs.ByteString Locales
  updLocWizardButton accum content aLocEntry = accum


analyseMenuItems :: Maybe CompLocales -> Maybe [Tt.ModelElement] -> [Tt.MenuItem] -> [Menu]
analyseMenuItems locales iconDefs =
  let
    menuLocales = case locales of
      Nothing -> Nothing
      Just aLocales -> maybe Nothing (Mp.lookup "name") (Mp.lookup "ir.ui.menu" aLocales.modelCL)
    iconMaps = buildIconDefMap iconDefs
  in
  map (analyseMenuItem menuLocales iconMaps)


buildIconDefMap :: Maybe [Tt.ModelElement] -> (Mp.Map T.Text (Mp.Map T.Text Tt.Field), Mp.Map T.Text T.Text)
buildIconDefMap mbDefs =
  case mbDefs of
    Nothing -> (Mp.empty, Mp.empty)
    Just defs ->
      let
        iconMap = Mp.fromList [ (aDef.idDF, aDef.fieldsDF) | aDef <- defs ]
        iconsByNameMap = Mp.fromList [(nameField.valueF, pathField.valueF) |
                                      aDef <- defs
                                      , Just nameField <- [Mp.lookup "name" aDef.fieldsDF]
                                      , Just pathField <- [Mp.lookup "path" aDef.fieldsDF]
                                    ]
      in
      (iconMap, iconsByNameMap)


analyseMenuItem :: Maybe (Mp.Map Bs.ByteString Locales) -> (Mp.Map T.Text (Mp.Map T.Text Tt.Field), Mp.Map T.Text T.Text) -> Tt.MenuItem -> Menu
analyseMenuItem locales (iconMap, iconsByNameMap) aMenuItem =
  let
    updLabel = case aMenuItem.nameMI of
      Nothing -> case locales of
        Nothing -> "anonymous"
        Just namedEntries ->
          case Mp.lookup (T.encodeUtf8 aMenuItem.idMI) namedEntries of
            Nothing -> "anonymous"
            Just aLocDef ->
              let
                values = Mp.toList aLocDef
              in
              if null values then
                "anonymous"
              else
                case head values of
                  (aKey, "") -> T.decodeUtf8 aKey
                  (aKey, aValue) -> T.decodeUtf8 aValue
      Just aString -> aString
    derefIcon = case aMenuItem.iconMI of
      Nothing -> Nothing
      Just iconName -> case iconName of
        "gnuhealth-list" -> Just "icons/gnuhealth-list.svg"
        _ -> case Mp.lookup (iconName <> "_icon") iconMap of
                Nothing -> Mp.lookup iconName iconsByNameMap
                Just fields -> Tt.valueF <$> Mp.lookup "path" fields
  in
  Menu {
    label = updLabel
  , icon = derefIcon
  , mid = aMenuItem.idMI
  , children = map (analyseMenuItem locales (iconMap, iconsByNameMap)) aMenuItem.childrenMI
  }


leftPartAItems :: [Menu] -> Bs.ByteString
leftPartAItems menus =
  let
    templ_1 = [r|module Protected.LeftMenuNav exposing (leftPartAItems)
import Protected.LeftMenuDef exposing (ItemLA (..))

leftPartAItems : List (ItemLA msg)
leftPartAItems = [|]
  in
  templ_1
    <> "\n" <> genMenus 0 menus
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


genFunctionDefs :: [Component] -> Bs.ByteString
genFunctionDefs components =
  Bs.intercalate "\n" $
    map (\aComp -> "  - id: " <> T.encodeUtf8 aComp.refID <> "\n    action:\n      Function:\n        DynRoutes: " <> T.encodeUtf8 aComp.refID) components





