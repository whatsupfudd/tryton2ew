module Generation.EwTypes where

import qualified Data.ByteString as Bs
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Mp
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Parsing.Python as Py
import qualified Generation.Elm as E
import qualified Generation.Utils as U

import qualified Tryton.Types as Tm


type Locales = Mp.Map Bs.ByteString Bs.ByteString
type ArrayLocales = Mp.Map Bs.ByteString [Bs.ByteString]

defaultLocales :: Locales
defaultLocales = Mp.empty

type ModelLocale = Mp.Map Bs.ByteString (Mp.Map Bs.ByteString (Mp.Map Bs.ByteString Locales))

data TranslateParams =
    ModelLK Bs.ByteString
  | FieldLK (Bs.ByteString, Bs.ByteString)
  | HelpLK
  | ReportLK
  | SelectionLK
  | ViewLK
  | WizardButtonLK
  | ErrorLK
  deriving (Show, Eq)


data LocalesPerKind = LocalesPerKind {
  -- models: <model-name> => <key>: name, desc, text, string => value => dict-key => dict-value
  modelCL :: ModelLocale
  , fieldCL :: Mp.Map Bs.ByteString (Mp.Map Bs.ByteString Bs.ByteString)
  , helpCL :: Mp.Map Bs.ByteString Locales
  , reportCL :: Mp.Map Bs.ByteString Locales
  -- selections: appointment_type, state, urgency, visit_type, ...
  , selectionCL :: Mp.Map Bs.ByteString (Mp.Map Bs.ByteString Locales)
  , viewCL :: Mp.Map Bs.ByteString Locales
  , wizardButtonCL :: Mp.Map Bs.ByteString Locales
  , errors :: [Bs.ByteString]
  }
  deriving Show
{-
parse:
 <kind>:<model-name>,<field-name>:? + <msgid==value>
 * ByteString

Gen:
 <phase of gen: model> -> insert translation ==> getValue by model-name.
 <phase of gen: field> -> getValue by model-name, field-name.
 --> output: bytestring | ActWin/Views/...: Text.
 --> Map T.Text 
-}

defaultLocalesPerKind = LocalesPerKind {
  modelCL = Mp.empty
  , fieldCL = Mp.empty
  , helpCL = Mp.empty
  , reportCL = Mp.empty
  , selectionCL = Mp.empty
  , viewCL = Mp.empty
  , wizardButtonCL = Mp.empty
  , errors = []
}


translateName :: Maybe LocalesPerKind -> TranslateParams -> Bs.ByteString
translateName mbLocales kind =
  case kind of
    ModelLK modelName ->
      maybe modelName (\locales ->
        fromMaybe modelName ( -- TODO: use the locale
          case Mp.lookup modelName locales.modelCL >>= Mp.lookup "name" >>= Mp.lookup ""  of
            Nothing -> Nothing
            Just cMap -> case Mp.keys cMap of
              [] -> Nothing
              (aKey : _) -> Just aKey
        )
      ) mbLocales
    FieldLK (modelName, fieldName) -> 
      maybe fieldName (\locales ->
          fromMaybe fieldName (
              Mp.lookup modelName locales.fieldCL >>= Mp.lookup (U.toLowerBs fieldName)
          )
        ) mbLocales
    -- TODO: implement other kinds of dereferencing for translation:
    _ -> "@[translateName] kind err: " <> (T.encodeUtf8 . T.pack . show) kind


data EwContext = EwContext {
  components :: [Component]    -- Implements the UI elements that the menus point to.
  , menus :: [Menu]           -- The menu items that go into the DesktopNav definition..
  , appEntries :: [AppEntry]  -- Endpoints that link events to reactions in the app.yaml definitions.
}


data Component = Component {
  path :: FilePath
  , moduleName :: Bs.ByteString
  , refID :: Bs.ByteString
  , types :: [E.TypeDef]
  , functions :: [E.FunctionDef]
  , locales :: Mp.Map T.Text Locales    -- keyword -> values per locale (en, fr, etc.)
  , fetchers :: [Fetcher]
  , inserters :: [Inserter]
  }
  deriving (Show)

data Menu = Menu {
  label :: Bs.ByteString
  , icon :: Maybe Bs.ByteString
  , mid :: Bs.ByteString
  , children :: [Menu]
  , action :: Maybe Bs.ByteString
  }
  deriving (Show)

data AppEntry = AppEntry {
  endpoint :: Bs.ByteString
  , dynLabel :: Bs.ByteString
}


data IconDef = IconDef {
  nameIc :: Bs.ByteString
  , pathIc :: Bs.ByteString
  }
  deriving (Show, Eq)


data ActionWindow = ActionWindow {
  idAW :: Bs.ByteString
  , logicNameAW :: Bs.ByteString
  , domainAW :: Maybe Bs.ByteString
  , contextAW :: Maybe Bs.ByteString
  , optionsAW :: [AwDomain]
  , viewLinksAW :: [(Bs.ByteString, Int)]  -- extracts from 'ir.action.act_window.view' => (ir.ui.view:name, sequence)
  , viewModelLinksAW :: Mp.Map Bs.ByteString (Bs.ByteString, Bs.ByteString)  -- 'ir.ui.view' => (viewDef name, viewDef type)
  , uiViewsAW :: Mp.Map Bs.ByteString Tm.Definition
  , logicView :: Maybe Py.TrytonModel
  }
  deriving (Show)


data AwDomain = AwDomain {
  nameAD :: Bs.ByteString
  , filterAD :: Maybe Bs.ByteString
  , sequenceAD :: Int
  }
  deriving (Show)


data Fetcher = Fetcher {
  midFT :: Bs.ByteString
  , handlerFT :: Bs.ByteString
  , functionFT :: Bs.ByteString
  , decoderFT :: Bs.ByteString
  , hsForwardFT :: Bs.ByteString
  , continuationFT :: Bs.ByteString
  }
  deriving (Show)

data HsForward = HsForward {
  nameHF :: Bs.ByteString
  , sqlFctHF :: Bs.ByteString
  , jsDecoderHF :: Bs.ByteString
  , jsEncoderHF :: Bs.ByteString
  , codeHF :: Bs.ByteString
  }
  deriving (Show)


data SqlFct = SqlFct {
  nameSF :: Bs.ByteString
  , codeSF :: Bs.ByteString
  }
  deriving (Show)

data Inserter = Inserter {
  midIn :: Bs.ByteString
  , nameIn :: Bs.ByteString
  }
  deriving (Show)

