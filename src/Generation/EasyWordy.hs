{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant maybe" #-}
{-# HLINT ignore "Use <|>" #-}
{-# HLINT ignore "Use =<<" #-}
module Generation.EasyWordy where

import qualified Data.ByteString as Bs
import qualified Data.Char as C
import Data.Either (rights, lefts, fromLeft, fromRight)
import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as T
import Text.RawString.QQ (r)

import System.FilePath ((</>))

import qualified Parsing.Xml as Xm
import qualified Parsing.Pot as Po
import qualified Parsing.Python as Pp
import qualified Generation.Elm as E
import qualified Generation.Views as Vw
import Generation.EwTypes


type UiDefs = ([Xm.MenuItem], Mp.Map T.Text [Xm.ClassInstance], Xm.ViewDefs)

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


generateApp :: FilePath -> UiDefs -> Po.LocaleDefs -> [(FilePath, [Pp.LogicElement])] -> IO (Either String ())
generateApp destPath uiDefs@(_, classInstances, viewDefs) locales logicElements =
  let
    consoLocales = consolidateLocales locales
    eiLeftMenuNav = genLeftMenu uiDefs consoLocales
  in
  case eiLeftMenuNav of
    Left err -> do
      pure $ Left err
    Right menus -> do
      let
        logicMap = foldl (\accum anElement ->
            case anElement of
              Pp.ModelEl trytonModel ->
                case Mp.lookup "v:__name__" trytonModel.fields of
                  Just aName -> case aName.value of
                    Pp.LiteralEx (Pp.StringLit str) -> Mp.insert (T.decodeUtf8 (Bs.drop 1 . Bs.init . mconcat $ str)) trytonModel accum
                    _ -> accum
                  Nothing -> accum
              _ -> accum
          ) Mp.empty (concatMap snd logicElements)
        (actionWindows, icons, errs) = scanInstances logicMap viewDefs (concat $ Mp.elems classInstances)
        components = genComponents actionWindows viewDefs menus consoLocales
        dynRoutes = genDynRoutes components
        yamlEntries = genFunctionDefs components
        context = EwContext {
          components = components
          , menus = menus
          , appEntries = []
        }
        renderedMenus = leftPartAItems menus
      TIO.writeFile (destPath </> "actWins.txt") $
        -- T.intercalate "\n" (map (T.pack . show) (Mp.toList logicMap))
        T.intercalate "\n" (map (T.pack . show) (Mp.elems actionWindows))
        <> "\n\n" <> T.intercalate "\n" (map T.pack errs)
      TIO.writeFile (destPath </> "wapp/Protected/LeftMenuNav.elm") (T.decodeUtf8 renderedMenus)
      TIO.writeFile (destPath </> "wapp/DynRoutes.elm") (T.decodeUtf8 dynRoutes)
      TIO.writeFile (destPath </> "yamlEntries.txt") $ T.decodeUtf8 yamlEntries
      TIO.writeFile (destPath </> "compLocales.txt") $ T.pack (show consoLocales)
      mapM_ (saveComponent destPath) components
      pure $ Right ()


data Pass1Accum = Pass1Accum {
  actWindowsP1 :: Mp.Map T.Text ActionWindow
  , iconsP1 :: Mp.Map T.Text IconDef
  , actDomainsP1 ::[Xm.ClassInstance]
  , actViewsP1 :: [Xm.ClassInstance]
  , irUiViewP1 :: [Xm.ClassInstance]
  }
  deriving (Show)


scanInstances :: Mp.Map T.Text Pp.TrytonModel -> Xm.ViewDefs -> [Xm.ClassInstance] -> (Mp.Map T.Text ActionWindow, Mp.Map T.Text IconDef, [String])
scanInstances logicMap viewDefs instances =
  let
    (p1Accum, p1Errs) = scanInstancePass1 instances
    (actWinMapByID, p2Errs) = scanInstancesPass2 p1Accum
    actWinMapByLogicName = Mp.fromList [ (aActWin.logicNameAW, aActWin) | aActWin <- Mp.elems actWinMapByID ]
    (updMap, p3Errs) = scanInstancesPass3 actWinMapByLogicName p1Accum.irUiViewP1
    (finalMap, p4Errs) = scanInstancesPass4 logicMap viewDefs updMap
  in
  {-
  case p1Errs <> p2Errs <> p3Errs of
    [] -> Right (Mp.fromList [ (aActWin.idAW, aActWin) | aActWin <- Mp.elems updMap ], p1Accum.iconsP1)
    errs -> Left $ "@[scanInstances] errs: " <> show errs
  -}
  (Mp.fromList [ (aActWin.idAW, aActWin) | aActWin <- Mp.elems finalMap ], p1Accum.iconsP1, p1Errs <> p2Errs <> p3Errs <> p4Errs)


-- Transforms ClassInstances into ActionWIndows and Icons.
scanInstancePass1 :: [Xm.ClassInstance] -> (Pass1Accum, [String])
scanInstancePass1 classInstances =
  let
    initAccum = Pass1Accum {
      actWindowsP1 = Mp.empty
      , iconsP1 = Mp.empty
      , actDomainsP1 = []
      , actViewsP1 = []
      , irUiViewP1 = []
    }
  in
  foldl (\(accum, errs) aModel -> case parseClassInstance accum aModel of
      Left err -> (accum, err : errs)
      Right newAccum -> (newAccum, errs)
    ) (initAccum, []) classInstances


-- Connects the act_window.domains and act_window.views to the ActionWindows.
scanInstancesPass2 :: Pass1Accum -> (Mp.Map T.Text ActionWindow, [String])
scanInstancesPass2 p1Accum =
  let
    domainConso = foldl (\(accum, errs) aDomain ->
        case Mp.lookup "act_window" aDomain.fieldsDF of
          Nothing ->
            let
              newErr = "No act_window for domain: " <> show aDomain
            in
            (accum, newErr : errs)
          Just actWinField ->
            case actWinField.kindF of
              Xm.ReferenceFK -> case Mp.lookup actWinField.valueF p1Accum.actWindowsP1 of
                Nothing ->
                  let
                    newErr = "No act_window for domain: " <> show aDomain
                  in
                  (accum, newErr : errs)
                Just actWin ->
                  case instanceToDomain aDomain of
                    Left err -> (accum, err : errs)
                    Right domain ->
                      let
                        newActWin = actWin { optionsAW = domain : actWin.optionsAW }
                      in
                      (Mp.insert actWinField.valueF newActWin accum, errs)
              _ ->
                let
                  newErr = "Unexpected act_window reference format: " <> show actWinField
                in
                (accum, newErr : errs)
      ) (Mp.empty, []) p1Accum.actDomainsP1
    in
    foldl (\(accum, errs) aView ->
      case Mp.lookup "act_window" aView.fieldsDF of
        Nothing ->
          let
            newErr = "No act_window field in act_window.view: " <> show aView
          in
          (accum, newErr : errs)
        Just actWinField ->
          case actWinField.kindF of
            Xm.ReferenceFK -> case Mp.lookup actWinField.valueF accum of
              Nothing ->
                case Mp.lookup actWinField.valueF p1Accum.actWindowsP1 of
                  Just actWin ->
                    makeValue (accum, errs) actWin actWinField aView
                  Nothing ->
                    let
                      newErr = "No ActionWindow for act_window.view: " <> show aView
                    in
                    (accum, newErr : errs)
              Just actWin ->
                makeValue (accum, errs) actWin actWinField aView
            _ ->
              let
                newErr = "Unexpected act_window reference format: " <> show actWinField
              in
              (accum, newErr : errs)
    ) domainConso p1Accum.actViewsP1
  where
  makeValue :: (Mp.Map T.Text ActionWindow, [String]) -> ActionWindow -> Xm.Field -> Xm.ClassInstance -> (Mp.Map T.Text ActionWindow, [String])
  makeValue (accum, errs) actWin actWinField aView =
    case Mp.lookup "view" aView.fieldsDF of
      Nothing ->
        let
          newErr = "No view field in act_window.view: " <> show aView
        in
        (accum, newErr : errs)
      Just viewField ->
        case viewField.kindF of
          Xm.ReferenceFK ->
            let
              newViewValue =
                case Mp.lookup "sequence" aView.fieldsDF of
                  Nothing -> Right (viewField.valueF, 0)
                  Just sequenceField ->
                    case sequenceField.kindF of
                      Xm.EvalFK -> Right (viewField.valueF, read $ T.unpack sequenceField.valueF)
                      _ -> Left $ "Unexpected sequence format in act_window.view: " <> show aView
            in
            case newViewValue of
              Left err -> (accum, err : errs)
              Right aPair ->
                let
                  newActWin = actWin { viewLinksAW = aPair : actWin.viewLinksAW }
                in
                (Mp.insert actWinField.valueF newActWin accum, errs)
          _ ->
            let
              newErr = "Unexpected view format in act_window.view: " <> show aView
            in
            (accum, newErr : errs)


-- Connects the ir.ui.views to the ActionWindows (based on the act_win.views relationships).
scanInstancesPass3 :: Mp.Map T.Text ActionWindow -> [Xm.ClassInstance] -> (Mp.Map T.Text ActionWindow, [String])
scanInstancesPass3 actWinMap =
  foldl (\(accum, errs) aUiView ->
    case Mp.lookup "model" aUiView.fieldsDF of
      Nothing ->
        case Mp.lookup "inherit" aUiView.fieldsDF of
          Nothing ->
            let
              newErr = "No model for ir.ui.view: " <> show aUiView
            in
            (accum, newErr : errs)
          _ -> (accum, errs)
      Just modelField -> case modelField.kindF of
        Xm.LabelFK ->
          case Mp.lookup modelField.valueF accum of
            Nothing ->
              let
                newErr = "No ActionWindow for ir.ui.view: " <> show aUiView
              in
              (accum, newErr : errs)
            Just actWin ->
              case Mp.lookup "name" aUiView.fieldsDF of
                Nothing ->
                  let
                    newErr = "No name for ir.ui.view: " <> show aUiView
                  in
                  (accum, newErr : errs)
                Just nameField ->
                  case Mp.lookup "type" aUiView.fieldsDF of
                    Nothing ->
                      let
                        newErr = "No type for ir.ui.view: " <> show aUiView
                      in
                      (accum, newErr : errs)
                    Just typeField ->
                      case typeField.kindF of
                        Xm.LabelFK ->
                          let
                            newValue = (nameField.valueF, typeField.valueF)
                            newActWin = actWin { viewModelLinksAW = Mp.insert aUiView.idDF newValue actWin.viewModelLinksAW }
                          in
                          (Mp.insert modelField.valueF newActWin accum, errs)
                        _ ->
                          let
                            newErr = "Unexpected type format in ir.ui.view: " <> show aUiView
                          in
                          (accum, newErr : errs)
        _ -> (accum, "Unexpected name format in instance: " <> show aUiView : errs)
  ) (actWinMap, [])


-- Connects the ModelViews (python) and view definitions (../views/*.xml) to the ActionWindows (based on the ir.ui.view relationships extracted in pass 3)
scanInstancesPass4 :: Mp.Map T.Text Pp.TrytonModel -> Xm.ViewDefs -> Mp.Map T.Text ActionWindow -> (Mp.Map T.Text ActionWindow, [String])
scanInstancesPass4 logicMap viewDefs actWinMap =
  let
    (updActWinMap, errs) = foldl (\(accum, errs) (k, anActWin) ->
        let
          mbTtModel = Mp.lookup anActWin.logicNameAW logicMap
          (uiViews, viewErrs) = foldl (\(accum, errs) (vName, vType) ->
              let
                mbViewDef = case vType of
                  "tree" -> uncurry Xm.TreeDF <$> Mp.lookup vName viewDefs.trees
                  "form" -> uncurry Xm.FormDF <$> Mp.lookup vName viewDefs.forms
                  -- TODO: other kinds of views.
                  _ -> Nothing
              in
              case mbViewDef of
                Nothing ->
                  let
                    errMsg = "No viewDef for: " <> T.unpack vName
                  in
                  (accum, errMsg : errs)
                Just viewDef -> ((vName, viewDef) : accum, errs)

            ) ([], []) anActWin.viewModelLinksAW
        in
        case (mbTtModel, uiViews) of
          (Nothing, []) -> (accum, "No logic nor ui.views for: " <> T.unpack k : errs)
          (Just ttModel, []) ->
            let
              newErr = "No ui.view for: " <> T.unpack k
              newActWin = anActWin { logicView = Just ttModel }
            in
              (Mp.insert k newActWin accum, newErr : errs)
          (mbTtModel, uiViews) ->
            let
              newActWin = anActWin { logicView = mbTtModel, uiViewsAW = Mp.fromList uiViews }
            in
            (Mp.insert k newActWin accum, errs)
      ) (Mp.empty, []) (Mp.toList actWinMap)
  in
  (updActWinMap, errs)


instanceToDomain :: Xm.ClassInstance -> Either String AwDomain
instanceToDomain anInstance =
  let
    eiName = case Mp.lookup "name" anInstance.fieldsDF of
      Nothing -> Left $ "No name for: " <> T.unpack anInstance.modelDF
      Just nameField -> case nameField.kindF of
        Xm.LabelFK -> Right nameField.valueF
        _ -> Left $ "Unexpected name format in instance: " <> show anInstance
    eiSequence = case Mp.lookup "sequence" anInstance.fieldsDF of
      Nothing -> Left $ "No sequence for: " <> show anInstance
      Just sequenceField -> case sequenceField.kindF of
        Xm.EvalFK -> Right (read $ T.unpack sequenceField.valueF)
        _ -> Left $ "Unexpected sequence format: " <> show sequenceField
    eiFilter = case Mp.lookup "domain" anInstance.fieldsDF of
      Nothing -> Right Nothing
      Just domainField -> case domainField.kindF of
        Xm.EvalFK -> Right (Just domainField.valueF)
        _ -> Left $ "Unexpected domain format in instance: " <> show anInstance
    lefties = fromLeft "" eiName <> fromLeft "" eiSequence <> fromLeft "" eiFilter
  in
  case lefties of
    "" -> Right $ AwDomain {
        nameAD = fromRight "" eiName
        , filterAD = fromRight Nothing eiFilter
        , sequenceAD = fromRight 0 eiSequence
      }
    _ -> Left $ "@[instanceToDomain] errors: " <> show lefties


parseClassInstance :: Pass1Accum -> Xm.ClassInstance -> Either String Pass1Accum
parseClassInstance accum aModel
  | T.isPrefixOf "ir." aModel.modelDF =
    let
      subModel = T.drop 3 aModel.modelDF
    in
    if T.isPrefixOf "action." subModel then
      let
        actionModel = T.drop 7 subModel
      in
      if T.isPrefixOf "act_window" actionModel then
        case T.drop 10 actionModel of
          "" ->
            case Mp.lookup "res_model" aModel.fieldsDF of
                Just lnField ->
                  case lnField.kindF of
                    Xm.LabelFK ->
                      let
                        newActWin = ActionWindow {
                          idAW = aModel.idDF
                          , logicNameAW = lnField.valueF
                          , domainAW = Nothing
                          , contextAW = Nothing
                          , optionsAW = []
                          , viewLinksAW = []
                          , viewModelLinksAW = Mp.empty
                          , uiViewsAW = Mp.empty
                          , logicView = Nothing
                        }
                      in
                      Right accum { actWindowsP1 = Mp.insert aModel.idDF newActWin accum.actWindowsP1 }
                    _ -> Left $ "Unexpected res_model format: " <> show lnField
                Nothing -> Left $ "No res_model for: " <> T.unpack aModel.modelDF
          ".domain" -> Right accum { actDomainsP1 = aModel : accum.actDomainsP1 }
          ".view" -> Right accum { actViewsP1 = aModel : accum.actViewsP1 }
          _ -> Left $ "Unknown ir.action.act_window. model: " <> T.unpack aModel.modelDF
      else case actionModel of
        -- TODO: handle these classes:
        "keyword" -> Right accum
        "report" -> Right accum
        "wizard" -> Right accum
        _ -> Left $ "Unknown ir.action. model: " <> T.unpack aModel.modelDF
    else if T.isPrefixOf "ui." subModel then
      case T.drop 3 subModel of
        "icon" ->
          let
            mbName = case Mp.lookup "name" aModel.fieldsDF of
              Nothing -> Nothing
              Just nameField -> case nameField.kindF of
                Xm.LabelFK -> Just nameField.valueF
                _ -> Nothing
            mbPath = case Mp.lookup "path" aModel.fieldsDF of
              Nothing -> Nothing
              Just pathField -> case pathField.kindF of
                Xm.LabelFK -> Just pathField.valueF
                _ -> Nothing
          in
          case (mbName, mbPath) of
            (Just name, Just path) -> Right accum { iconsP1 = Mp.insert aModel.idDF (IconDef name path) accum.iconsP1 }
            _ -> Left $ "Unexpected icon format: " <> show aModel
        "view" -> Right accum { irUiViewP1 = aModel : accum.irUiViewP1 }
        "menu-res.group" -> Right accum
        _ -> Left $ "Unknown ir.ui. model: " <> T.unpack aModel.modelDF
    else if T.isPrefixOf "model." subModel then
      case T.drop 6 subModel of
        -- TODO: handle these classes:
        "access" -> Right accum
        "button" -> Right accum
        "field.access" -> Right accum
        "button-res.group" -> Right accum
        _ -> Left $ "Unknown ir.model. model: " <> T.unpack aModel.modelDF
    else if T.isPrefixOf "sequence" subModel then
      case T.drop 8 subModel of
        -- TODO: handle these classes:
        "" -> Right accum
        ".type" -> Right accum
        _ -> Left $ "Unknown ir.sequence. model: " <> T.unpack aModel.modelDF
    else if T.isPrefixOf "rule" subModel || (subModel == "message") then
      Right accum
    else
      Left $ "Unknown ir. model: " <> T.unpack aModel.modelDF
  | T.isPrefixOf "gnuhealth." aModel.modelDF =
    let
      ghModel = T.drop 10 aModel.modelDF
    in
    if T.isPrefixOf "drug." ghModel then
      case T.drop 5 ghModel of
        -- TODO: handle these classes:
        "form" -> Right accum
        "route" -> Right accum
        _ -> Left $ "Unknown gnuhealth.drug. model: " <> T.unpack aModel.modelDF
    else if T.isPrefixOf "body_function" ghModel
        || T.isPrefixOf "activity_and_participation" ghModel
        || T.isPrefixOf "body_structure" ghModel
        || T.isPrefixOf "diet." ghModel
        || T.isPrefixOf "disease_group" ghModel
        || T.isPrefixOf "drugs_recreational" ghModel
        || T.isPrefixOf "environmental_factor" ghModel
        || T.isPrefixOf "federation." ghModel
        || T.isPrefixOf "gene" ghModel
        || T.isPrefixOf "imaging." ghModel
        || T.isPrefixOf "lab." ghModel
        || T.isPrefixOf "medicament" ghModel
        || T.isPrefixOf "pathology" ghModel
        || T.isPrefixOf "pediatrics.growth.charts.who" ghModel
        || T.isPrefixOf "procedure" ghModel
        || T.isPrefixOf "vegetarian_types" ghModel
      then
      -- TODO: handle these classes:
      Right accum
    else case ghModel of
      -- TODO: handle these classes:
      "command" -> Right accum
      "dose.unit" -> Right accum
      "ethnicity" -> Right accum
      "help" -> Right accum
      "medication.dosage" -> Right accum
      "occupation" -> Right accum
      "pathology.group" -> Right accum
      "specialty" -> Right accum
      "surgery.protocol" -> Right accum
      "protein.disease" -> Right accum
      "dentistry.procedure" -> Right accum
      _ -> Left $ "Unknown gnuhealth. model: " <> T.unpack aModel.modelDF
  | T.isPrefixOf "res." aModel.modelDF = case T.drop 4 aModel.modelDF of
      -- TODO: handle these classes:
      "group" -> Right accum
      "user" -> Right accum
      "user-res.group" -> Right accum
      _ -> Left $ "Unknown res. model: " <> T.unpack aModel.modelDF
  -- TODO: handle these classes:
  | T.isPrefixOf "product." aModel.modelDF = Right accum
  | otherwise = Left $ "Unknown top-level model: " <> T.unpack aModel.modelDF


genLeftMenu :: UiDefs -> Mp.Map Bs.ByteString CompLocales -> Either String [Menu]
genLeftMenu (menuItems, modelDefs, _) locales =
  let
    enLocales = Mp.lookup "en" locales
    iconDefs = Mp.lookup "ir.ui.icon" modelDefs
    menus = analyseMenuItems enLocales iconDefs menuItems
  in
  Right menus


genComponents :: Mp.Map T.Text ActionWindow -> Xm.ViewDefs -> [Menu] -> Mp.Map Bs.ByteString CompLocales -> [Component]
genComponents actionWindows viewDefs leftMenus locales =
  concatMap (\aMenu ->
      let
        componentName = convertMenuID aMenu.mid
        mbActionWindow = case aMenu.action of
          Nothing -> Nothing
          Just actionID -> Mp.lookup actionID actionWindows
        fileName = "wapp/Components/" <> T.unpack componentName <> ".elm"
        topComp = Component {
              path = fileName
            , moduleName = componentName
            , refID = aMenu.mid
            , types = []
            , functions = genFunction mbActionWindow
            , locales = Mp.empty  -- Fix this.
          }
        childrenComponents = genComponents actionWindows viewDefs aMenu.children locales
      in
      topComp : childrenComponents
    ) leftMenus


genFunction :: Maybe ActionWindow -> [E.FunctionDef]
genFunction mbActionWin =
  case mbActionWin of
    Nothing ->
      [
        E.FunctionDef {
          nameFD = "demoFct"
          , argsFD = []
          , typeDef = E.StringTD
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
          Xm.TreeDF attribs elements ->
            Vw.genTree vName elements : accum
          Xm.FormDF {} ->
            accum
          ) [] (Mp.toList actionWin.uiViewsAW)
      in
      [
        E.FunctionDef {
          nameFD = "demoFct"
          , argsFD = []
          , typeDef = E.StringTD
          , bodyFD = E.div [E.class_ "bg-gray-50 dark:bg-gray-900 p-4 md:ml-64 lg:mr-16 min-h-full pt-20" ] $ case subFcts of
              [] -> [ E.div [E.class_ "text-red-500" ] [ E.text actionWin.logicNameAW ] ]
              _ -> [
                E.div [ E.class_ "mx-auto max-w-screen-xl px-4 lg:px-12" ]
                  ([ E.div [ E.class_ "flex justify-between items-center mb-4" ]
                    [ E.h1 [ E.class_ "text-2xl text-gray-900 dark:text-white font-bold" ] [ E.text actionWin.logicNameAW ]
                    , E.div [ E.class_ "flex items-center" ]
                      [ E.button [ E.class_ "bg-gray-500 text-white px-4 py-2 rounded-md" ] [ E.text "Refresh" ] ]
                    ]
                  ]
                  <> [ E.ApplyEE aFct.nameFD [] | aFct <- subFcts ]
                  )
                ]
        }    
      ] <> subFcts


genSqlCode :: [Xm.Definition] -> IO (Either String ())
genSqlCode defs = pure $ Right ()


convertMenuID :: T.Text -> T.Text
convertMenuID oriName =
  let
    nameParts = concatMap (T.splitOn "_") (concatMap (T.splitOn "-") (T.splitOn "." oriName))
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

|]
{-
demoFct =
  div [ class "bg-gray-50 dark:bg-gray-900 p-4 md:ml-64 lg:mr-16 min-h-full pt-20" ] [ div [ class "text-red-500" ] [ text "
-}
  in
  "module Components." <> T.encodeUtf8 component.moduleName
  <> templ_1
  <> Bs.intercalate "\n" (map E.spitFct component.functions)
 -- <> T.encodeUtf8 component.moduleName <> "\" ]]\n"

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


analyseMenuItems :: Maybe CompLocales -> Maybe [Xm.ClassInstance] -> [Xm.MenuItem] -> [Menu]
analyseMenuItems locales iconDefs =
  let
    menuLocales = case locales of
      Nothing -> Nothing
      Just aLocales -> maybe Nothing (Mp.lookup "name") (Mp.lookup "ir.ui.menu" aLocales.modelCL)
    iconMaps = buildIconDefMap iconDefs
  in
  map (analyseMenuItem menuLocales iconMaps)


buildIconDefMap :: Maybe [Xm.ClassInstance] -> (Mp.Map T.Text (Mp.Map T.Text Xm.Field), Mp.Map T.Text T.Text)
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


analyseMenuItem :: Maybe (Mp.Map Bs.ByteString Locales) -> (Mp.Map T.Text (Mp.Map T.Text Xm.Field), Mp.Map T.Text T.Text) -> Xm.MenuItem -> Menu
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
          Just fields -> Xm.valueF <$> Mp.lookup "path" fields
  in
  Menu {
    label = updLabel
  , icon = derefIcon
  , mid = aMenuItem.idMI
  , children = map (analyseMenuItem locales (iconMap, iconsByNameMap)) aMenuItem.childrenMI
  , action = aMenuItem.actionMI
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





