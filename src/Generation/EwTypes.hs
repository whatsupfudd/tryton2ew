module Generation.EwTypes where

import qualified Data.ByteString as Bs
import qualified Data.Map.Strict as Mp
import qualified Data.Text as T

import qualified Parsing.Python as Py
import qualified Generation.Elm as E

import qualified Tryton.Types as Tm


type Locales = Mp.Map Bs.ByteString Bs.ByteString

defaultLocales :: Locales
defaultLocales = Mp.empty

type ModelLocale = Mp.Map Bs.ByteString (Mp.Map Bs.ByteString (Mp.Map Bs.ByteString Locales))

data CompLocales = CompLocales {
  -- models: <model-name> => <key>: name, desc, text, string => value => dict-key => dict-value
  modelCL :: ModelLocale
  , fieldCL :: Mp.Map Bs.ByteString (Mp.Map Bs.ByteString Locales)
  , helpCL :: Mp.Map Bs.ByteString Locales
  , reportCL :: Mp.Map Bs.ByteString Locales
  -- selections: appointment_type, state, urgency, visit_type, ...
  , selectionCL :: Mp.Map Bs.ByteString (Mp.Map Bs.ByteString Locales)
  , viewCL :: Mp.Map Bs.ByteString Locales
  , wizardButtonCL :: Mp.Map Bs.ByteString Locales
  , errors :: [Bs.ByteString]
  }
  deriving Show


defaultCompLocales = CompLocales {
  modelCL = Mp.empty
  , fieldCL = Mp.empty
  , helpCL = Mp.empty
  , reportCL = Mp.empty
  , selectionCL = Mp.empty
  , viewCL = Mp.empty
  , wizardButtonCL = Mp.empty
  , errors = []
}


data EwContext = EwContext {
  components :: [Component]    -- Implements the UI elements that the menus point to.
  , menus :: [Menu]           -- The menu items that go into the DesktopNav definition..
  , appEntries :: [AppEntry]  -- Endpoints that link events to reactions in the app.yaml definitions.
}


data Component = Component {
  path :: FilePath
  , moduleName :: T.Text
  , refID :: T.Text
  , types :: [E.TypeDef]
  , functions :: [E.FunctionDef]
  , locales :: Mp.Map T.Text Locales    -- keyword -> values per locale (en, fr, etc.)
  , fetchers :: [Fetcher]
  , inserters :: [Inserter]
  }
  deriving (Show)

data Menu = Menu {
  label :: T.Text
  , icon :: Maybe T.Text
  , mid :: T.Text
  , children :: [Menu]
  , action :: Maybe T.Text
  }
  deriving (Show)

data AppEntry = AppEntry {
  endpoint :: T.Text
  , dynLabel :: T.Text
}


data IconDef = IconDef {
  nameIc :: T.Text
  , pathIc :: T.Text
  }
  deriving (Show, Eq)


data ActionWindow = ActionWindow {
  idAW :: T.Text
  , logicNameAW :: T.Text
  , domainAW :: Maybe T.Text
  , contextAW :: Maybe T.Text
  , optionsAW :: [AwDomain]
  , viewLinksAW :: [(T.Text, Int)]  -- extracts from 'ir.action.act_window.view' => (ir.ui.view:name, sequence)
  , viewModelLinksAW :: Mp.Map T.Text (T.Text, T.Text)  -- 'ir.ui.view' => (viewDef name, viewDef type)
  , uiViewsAW :: Mp.Map T.Text Tm.Definition
  , logicView :: Maybe Py.TrytonModel
  }
  deriving (Show)


data AwDomain = AwDomain {
  nameAD :: T.Text
  , filterAD :: Maybe T.Text
  , sequenceAD :: Int
  }
  deriving (Show)


data Fetcher = Fetcher {
  midFT :: Bs.ByteString
  , handlerFT :: T.Text
  , functionFT :: T.Text
  , decoderFT :: T.Text
  , hsForwardFT :: T.Text
  , continuationFT :: T.Text
  }
  deriving (Show)

data HsForward = HsForward {
  nameHF :: Bs.ByteString
  , sqlFctHF :: T.Text
  , jsDecoderHF :: T.Text
  , jsEncoderHF :: T.Text
  , codeHF :: Bs.ByteString
  }
  deriving (Show)


data SqlFct = SqlFct {
  nameSF :: Bs.ByteString
  , codeSF :: Bs.ByteString
  }
  deriving (Show)

data Inserter = Inserter {
  midIn :: T.Text
  , nameIn :: T.Text
  }
  deriving (Show)

