module Generation.SqlAst where

import qualified Data.Text as T

data Field = Field {
  name :: T.Text,
  type_ :: (T.Text, Bool),
  tableSynonym :: Maybe T.Text
}

data Statement =
  InsertStatement InsertST
  | UpdateStatement UpdateST
  | DeleteStatement DeleteST
  | SelectStatement SelectST

data InsertST = InsertST {
  destTable :: T.Text
  , fields :: [Field]
  , values :: [T.Text]
  , whereClause :: Maybe T.Text
  , returning :: [Field]
}

data UpdateST = UpdateST {
  updateTable :: T.Text,
  updateFields :: [Field]
}

data DeleteST = DeleteST {
  deleteTable :: T.Text,
  whereClause :: Maybe T.Text
}

data SelectST = SelectST {
  selectFields :: [Field],
  fromTables :: [T.Text],
  whereClause :: Maybe T.Text,
  orderBy :: [Field],
  limit :: Maybe Int,
  offset :: Maybe Int
}