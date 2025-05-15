module Generation.Fuddle where

import qualified Data.Map.Strict as Mp
import qualified Data.Text as T

type Locales = Mp.Map T.Text T.Text

data Component = Component {
  path :: FilePath
  , moduleName :: T.Text
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
