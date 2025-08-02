module Tryton.Types where

import qualified Data.ByteString as Bs
import Data.Text (Text)
import qualified Data.Map.Strict as Mp

import qualified Data.ConfigFile as Cf
import qualified Parsing.Python as Py
import qualified Generation.Sql as Sq
import qualified Parsing.Pot as Po


data ModuleSrcTT = ModuleSrcTT {
  nameMT :: String
  , locationMT :: Text
  -- From config:
  , dependsMT :: [String]
  , targetMT :: Maybe FilePath
  , dataSpecMT :: [FilePath]
  , supportSpecMT :: [FilePath]
  -- From dir walk:
  , localesMT :: [FilePath]
  , viewsMT :: [FilePath]
  , miscXmlMT :: [FilePath]
  , logicMT :: [FilePath]
  }
  deriving (Show)


data FullModuleTT = FullModuleTT {
  srcModuleFM :: ModuleSrcTT
  , menusFM :: [MenuItem]
  , actWinsFM :: [ModelInstance]
  , xmlDefsFM :: [XmlDefs]
  , localesFM :: Mp.Map Bs.ByteString [Po.LocEntry]
  , viewDefsFM :: Maybe ViewDefs
  , logicFM :: Mp.Map Text [Py.LogicElement]
  , sqlDefsFM :: Mp.Map Text Sq.SqlTable
  }
  deriving (Show)


data TrytonApp = TrytonApp {
  modulesTA :: [FullModuleTT]
  , localesTA :: Po.LocaleForModule
  , menuTreeTA :: [MenuItem]
  , instancesByKindTA :: Mp.Map Text [ModelInstance]
  }
  deriving (Show)


data TargetFileKind =
    XmlFile
  | PotFile
  | PyFile
  | CfgFile
  deriving (Show, Eq)


data TargetFile = TargetFile {
    kindTF :: !TargetFileKind
  , pathTF :: !FilePath
  }


-- Used to be in Parsing.Xml:
type XmlDefs = ([MenuItem], Mp.Map Text [ModelInstance], ViewDefs)

data MenuItem = MenuItem {
    idMI :: Text
  , parentMI :: Maybe Text
  , namespaceMI :: Maybe Text
  , nameMI :: Maybe Text
  , iconMI :: Maybe Text
  , seqOrdMI :: Int
  , childrenMI :: [MenuItem]
  , actionMI :: Maybe Text
  }
  deriving (Show)


data ViewDefs = ViewDefs {
    trees :: Mp.Map Text ([Attribute], [TreeElement])
  , forms :: Mp.Map Text ([Attribute], [FormElement])
  , lists :: Mp.Map Text (DefContent ToDoElement)
  , graphs :: Mp.Map Text ([Attribute], [ToDoElement])
  , boards :: Mp.Map Text ([Attribute], [ToDoElement])
  , calendars :: Mp.Map Text ([Attribute], [ToDoElement])
  , errors :: Mp.Map Text [String]
  }
  deriving (Show)

instance Semigroup ViewDefs where
  (ViewDefs aA aB aC aD aE aF aG) <> (ViewDefs bA bB bC bD bE bF bG) = ViewDefs (aA <> bA) (aB <> bB) (aC <> bC) (aD <> bD) (aE <> bE) (aF <> bF) (aG <> bG)

emptyViewDefs :: ViewDefs
emptyViewDefs = ViewDefs {
  trees = Mp.empty
  , forms = Mp.empty
  , lists = Mp.empty
  , graphs = Mp.empty
  , boards = Mp.empty
  , calendars = Mp.empty
  , errors = Mp.empty
  }

{-
  length (Mp.elems (lists aViewDefs)),
  length (Mp.elems (graphs aViewDefs)),
  length (Mp.elems (boards aViewDefs)),
  length (Mp.elems (calendars aViewDefs))
-}
lengthViewDefs :: ViewDefs -> (Int, Int)
lengthViewDefs aViewDefs =
  (length (Mp.elems (trees aViewDefs)), length (Mp.elems (forms aViewDefs)))

data Definition =
  ModelDF ModelInstance
  | TreeDF [Attribute] [TreeElement]
  | FormDF [Attribute] [FormElement]
  | ListFormDF (DefContent ToDoElement)
  | GraphDF [Attribute] [ToDoElement]
  | BoardDF [Attribute] [ToDoElement]
  | CalendarDF [Attribute] [ToDoElement]
  | ParseErrorDF String
  deriving (Show)

data DefContent eleT = DefContent {
    elements :: [eleT]
  , attributes :: [Attribute]
  }
  deriving (Show)

instance Semigroup (DefContent eleT) where
  (DefContent aA aB) <> (DefContent bA bB) = DefContent (aA <> bA) (aB <> bB)


data ModelInstance = ModelInstance {
    idDF     :: !Text
  , modelDF  :: !Text
  , fieldsDF :: !(Mp.Map Text Field)
  }
  deriving (Show)


data TreeElement =
  FieldTE [Attribute] [NFixElement]
  | ButtonTE [Attribute]
  deriving (Show)

data NFixElement =
  PrefixNF [Attribute]
  | SuffixNF [Attribute]
  deriving (Show)


data FormElement =
  LabelFE [Attribute]
  | FieldFE [Attribute]
  | ImageFE [Attribute]
  | SeparatorFE [Attribute]
  | NewlineFE [Attribute]
  | ButtonFE [Attribute]
  | LinkFE [Attribute]
  | NotebookFE [Attribute] [FormElement]
  | PageFE [Attribute] [FormElement]
  | GroupFE [Attribute] [FormElement]
  | HPanedFE [Attribute] [FormElement]
  | VPanedFE [Attribute] [FormElement]
  | ChildFE [Attribute]
  deriving (Show)

data Attribute = Attribute {
      nameA :: Text
    , valueA :: Text
  }
  deriving (Show)

-- TODO: implement all the elements for ListForm, Graph, Board and Calendar.
data ToDoElement = ToDoElement { nameTDE :: Text, childrenTDE :: [ToDoElement] }
  deriving (Show)


data FieldKind =
  LabelFK
  | EvalFK
  | ReferenceFK
  deriving (Show)


data Field = Field {
    kindF :: !FieldKind
  , valueF :: !Text
  }
  deriving (Show)
