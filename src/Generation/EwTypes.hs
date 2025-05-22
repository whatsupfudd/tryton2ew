module Generation.EwTypes where

import qualified Data.Map.Strict as Mp
import qualified Data.Text as T


type Locales = Mp.Map T.Text T.Text

defaultLocales :: Locales
defaultLocales = Mp.empty

type ModelLocale = Mp.Map T.Text (Mp.Map T.Text (Mp.Map T.Text Locales))

data CompLocales = CompLocales {
  -- models: <model-name> => <key>: name, desc, text, string => value => dict-key => dict-value
  modelCL :: ModelLocale
  , fieldCL :: Mp.Map T.Text (Mp.Map T.Text Locales)
  , helpCL :: Mp.Map T.Text Locales
  , reportCL :: Mp.Map T.Text Locales
  -- selections: appointment_type, state, urgency, visit_type, ...
  , selectionCL :: Mp.Map T.Text (Mp.Map T.Text Locales)
  , viewCL :: Mp.Map T.Text Locales
  , wizardButtonCL :: Mp.Map T.Text Locales
  , errors :: [T.Text]
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


