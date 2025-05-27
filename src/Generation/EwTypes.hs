module Generation.EwTypes where

import qualified Data.ByteString as Bs
import qualified Data.Map.Strict as Mp
import qualified Data.Text as T


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
  , types :: [TypeDef]
  , functions :: [FunctionDef]
  , locales :: Mp.Map T.Text Locales    -- keyword -> values per locale (en, fr, etc.)
}

data TypeDef = TypeDef {
  name :: T.Text
  , fields :: [FieldDef]
}

data FieldDef = FieldDef {
  name :: T.Text
  , typing :: TypeDef
}

data FunctionDef = FunctionDef {
  name :: T.Text
  , parameters :: [ParameterDef]
  , returnType :: TypeDef
}

data ParameterDef = ParameterDef {
  name :: T.Text
  , typing :: TypeDef
}


data Menu = Menu {
  label :: T.Text
  , icon :: Maybe T.Text
  , mid :: T.Text
  , children :: [Menu]
}

data AppEntry = AppEntry {
  endpoint :: T.Text
  , dynLabel :: T.Text
}


data TableDef = TableDef {
  name :: T.Text
  , comment :: Maybe T.Text
  , columns :: [ColumnDef]
}

data ColumnDef = ColumnDef {
  name :: T.Text
  , typing :: TypeDef
  , comment :: Maybe T.Text
}


