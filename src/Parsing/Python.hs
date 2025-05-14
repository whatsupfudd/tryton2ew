{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
-- RankNTypes enables the 'forall', which in turns qualifies the 'annot' type variable to be a class of 'Show'.
module Parsing.Python where

import Data.Either (rights)

import qualified Language.Python.Version3.Parser as Pp
import qualified Language.Python.Common as Pc
import Data.List (intercalate)
import Data.Maybe (mapMaybe)


data Element =
  ClassEl String
  | ModelEl Model
  | AssignEl Assign
  deriving (Eq)

instance Show Element where
  show e =
    case e of
      ModelEl m ->
        "Model: " <> m.name
          <> " (super: " <> intercalate ", " m.superclasses <> ")\n  "
          <> intercalate "\n  " (map show m.fields)
          <> "\n  " <> intercalate "\n  " (map show m.body)
          <> "\n"
      AssignEl a ->
        "Assign: " <> intercalate "." (map show a.target) <> " = " <> show a.value <> "\n"
      ClassEl c ->
        "Class: " <> c <> "\n"


data Model = Model {
  name :: String
  , superclasses :: [String]
  , fields :: [Field]
  , body :: [Statement]
} deriving (Show, Eq)


data ClassStmt =
  FieldCS Assign
  | StatementCS Statement
  deriving (Show, Eq)


data Field = Field {
    name :: String
    , value :: Expr
  }
  deriving (Show, Eq)


data Statement =
  Comment [String]
  | Other Expr
  deriving (Eq)

instance Show Statement where
  show stmt =
    case stmt of
      Comment strs ->
        "Comment: " <> intercalate "\n" strs
      Other expr ->
        "Other: " <> show expr


data Assign = Assign {
  target :: [ Expr ]
  , value :: Expr
} deriving (Show, Eq)

data Expr =
  VarEx String
  | LiteralEx Literal
  | CallEx Expr [Argument]
  | LambdaEx Expr
  | ArrayEx [Expr]
  | MapEx [Expr]
  | ParenEx Expr
  | BinOpEx String Expr Expr
  | UniOpEx String Expr
  | DotEx Expr String
  | TupleEx [Expr]
  | CondEx Expr Expr Expr
  | DictEx Expr Expr
  | ShownEx String
  deriving (Eq)

instance Show Expr where
  show expr =
    case expr of
      VarEx s ->
        "v:" <> s
      LiteralEx lit ->
        "lt:" <> show lit
      CallEx func args ->
        "c:" <> show func <> "(" <> intercalate ", " (map show args) <> ")"
      LambdaEx expr ->
        "lx: " <> show expr
      ArrayEx exprs ->
        "a:[" <> intercalate ", " (map show exprs) <> "]"
      MapEx exprs ->
        "m:{" <> intercalate ", " (map show exprs) <> "}"
      ParenEx expr ->
        "p:(" <> show expr <> ")"
      BinOpEx op left right ->
        "b:" <> show left <> " " <> show op <> " " <> show right
      UniOpEx op expr ->
        "u:" <> show op <> " " <> show expr
      DotEx expr attr ->
        "d:" <> show expr <> "." <> attr
      TupleEx exprs ->
        "t:[" <> intercalate ", " (map show exprs) <> "]"
      CondEx cond trueBranch falseBranch ->
        "cond:(" <> show cond <> " ? " <> show trueBranch <> " : " <> show falseBranch <> ")"
      DictEx key datum ->
        "dict:(" <> show key <> " => " <> show datum <> ")"
      ShownEx s ->
        "s:" <> s


data Literal =
  IntLit Integer
  | LIntLit Integer
  | FloatLit Double
  | StringLit [String]
  | BoolLit Bool
  | NoneLit
  | EllipsisLit
  deriving (Eq)

instance Show Literal where
  show lit =
    case lit of
      IntLit n ->
        show n
      LIntLit n ->
        show n
      FloatLit f ->
        show f
      StringLit strs ->
        intercalate "++" strs
      BoolLit b ->
        show b
      NoneLit ->
        "None"
      EllipsisLit ->
        "..."


data Argument =
  VarArg String
  | LambdaArg Expr
  | NamedArg String Expr
  deriving (Eq)

instance Show Argument where
  show arg =
    case arg of
      VarArg s ->
        "av:" <> s
      LambdaArg expr ->
        "al: " <> show expr
      NamedArg name expr ->
        "an:" <>name <> "=" <> show expr


extractElements :: FilePath -> IO [Element]
extractElements filePath = do
  content <- readFile filePath
  let
    eiModule = Pp.parseModule content filePath
  case eiModule of
    Left err -> do
      putStrLn $ "Error parsing file: " <> filePath <> " " <> show err
      pure []
    Right (Pc.Module statements, _) ->
      pure $ analyzeStatements statements


analyzeStatements :: forall annot. Show annot => [Pc.Statement annot] -> [Element]
analyzeStatements statements =
  let
    eiModels = map analyzeTopStmt statements
  in
  rights eiModels


analyzeTopStmt :: forall annot. Show annot => Pc.Statement annot -> Either String Element
analyzeTopStmt statement =
  case statement of
    Pc.Class { class_name = name, class_args = args, class_body = body } ->
      let
        superclasses = mapMaybe decodeArg args
      in
      if "ModelSQL" `elem` superclasses then
        let
          classStmts = mapMaybe analyzeClassStmt body
          fields = extractFields classStmts
          stmts = extractStmts classStmts
        in
        Right . ModelEl $ Model {
              name = name.ident_string
            , superclasses = mapMaybe decodeArg args
            , fields = fields
            , body = stmts
          }
      else
        Right $ ClassEl name.ident_string
    Pc.Assign { assign_to = target, assign_expr = expr } ->
      Right $ AssignEl $ Assign { target = map decodeTargetExpr target, value = evalExpr expr }
    _ ->
      Left "Not a class"


extractFields :: [ClassStmt] -> [Field]
extractFields =
  foldr (\cs accum ->
    case cs of
      FieldCS assignEx ->
        let
          fileName =
            if all isStringLiteral assignEx.target then
              intercalate "." (concatMap extractStringLiteral assignEx.target)
            else
              intercalate "." (map show assignEx.target)
        in
        Field { name = fileName, value = assignEx.value } : accum
      StatementCS _ -> accum
  ) []


extractStmts :: [ClassStmt] -> [Statement]
extractStmts =
  foldr (\cs accum ->
    case cs of
      FieldCS _ -> accum
      StatementCS s -> s : accum
  ) []


isField :: ClassStmt -> Bool
isField (FieldCS _) = True
isField _ = False

isStatement :: ClassStmt -> Bool
isStatement (StatementCS _) = True
isStatement _ = False

{-
The class statements in the AST that we are interested in are:
  Assign
      { assign_to :: [Expr annot] -- ^ Entity to assign to. 
      , assign_expr :: Expr annot -- ^ Expression to evaluate.
      , stmt_annot :: annot
      }
  | StmtExpr { stmt_expr :: Expr annot, stmt_annot :: annot }
-}
analyzeClassStmt :: forall annot. Show annot => Pc.Statement annot -> Maybe ClassStmt
analyzeClassStmt statement =
  case statement of
    Pc.Assign { assign_to = target, assign_expr = expr } ->
      Just . FieldCS $ Assign { target = map decodeTargetExpr target, value = evalExpr expr }
    Pc.StmtExpr { stmt_expr = expr } ->
      if isPyStringLiteral expr then
        let
          strLit = extractStringLiteral (evalExpr expr)
        in
        Just . StatementCS $ Comment strLit
      else
        Just . StatementCS $ Other (evalExpr expr)
    _ ->
      Nothing


isPyStringLiteral :: Pc.Expr annot -> Bool
isPyStringLiteral expr =
  case expr of
    Pc.ByteStrings _ _ -> True
    Pc.Strings _ _ -> True
    Pc.UnicodeStrings _ _ -> True
    _ -> False


isStringLiteral :: Expr -> Bool
isStringLiteral (LiteralEx (StringLit _)) = True
isStringLiteral _ = False

extractStringLiteral :: Expr -> [String]
extractStringLiteral expr =
  case expr of
    LiteralEx (StringLit strings) -> map removeQuotes strings
    _ -> []

removeQuotes :: String -> String
removeQuotes =
  dropWhile (== '\'') . reverse . dropWhile (== '\'') . reverse

{-
The AST defines the expressions as:
data Expr annot
   -- | Variable.
   = Var { var_ident :: Ident annot, expr_annot :: annot }
   -- | Literal integer.
   | Int { int_value :: Integer, expr_literal :: String, expr_annot :: annot }
   -- | Long literal integer. /Version 2 only/.
   | LongInt { int_value :: Integer, expr_literal :: String, expr_annot :: annot }
   -- | Literal floating point number.
   | Float { float_value :: Double, expr_literal :: String, expr_annot :: annot }
   -- | Literal imaginary number.
   | Imaginary { imaginary_value :: Double, expr_literal :: String, expr_annot :: annot }
   -- | Literal boolean.
   | Bool { bool_value :: Bool, expr_annot :: annot }
   -- | Literal \'None\' value.
   | None { expr_annot :: annot }
   -- | Ellipsis \'...\'.
   | Ellipsis { expr_annot :: annot }
   -- | Literal byte string.
   | ByteStrings { byte_string_strings :: [String], expr_annot :: annot }
   -- | Literal strings (to be concatentated together).
   | Strings { strings_strings :: [String], expr_annot :: annot }
   -- | Unicode literal strings (to be concatentated together). Version 2 only.
   | UnicodeStrings { unicodestrings_strings :: [String], expr_annot :: annot }
   -- | Function call. 
   | Call
     { call_fun :: Expr annot -- ^ Expression yielding a callable object (such as a function).
     , call_args :: [Argument annot] -- ^ Call arguments.
     , expr_annot :: annot
     }
   -- | Subscription, for example \'x [y]\'. 
   | Subscript { subscriptee :: Expr annot, subscript_expr :: Expr annot, expr_annot :: annot }
   -- | Slicing, for example \'w [x:y:z]\'. 
   | SlicedExpr { slicee :: Expr annot, slices :: [Slice annot], expr_annot :: annot }
   -- | Conditional expresison. 
   | CondExpr
     { ce_true_branch :: Expr annot -- ^ Expression to evaluate if condition is True.
     , ce_condition :: Expr annot -- ^ Boolean condition.
     , ce_false_branch :: Expr annot -- ^ Expression to evaluate if condition is False.
     , expr_annot :: annot
     }
   -- | Binary operator application.
   | BinaryOp { operator :: Op annot, left_op_arg :: Expr annot, right_op_arg :: Expr annot, expr_annot :: annot }
   -- | Unary operator application.
   | UnaryOp { operator :: Op annot, op_arg :: Expr annot, expr_annot :: annot }
   -- Dot operator (attribute selection)
   | Dot { dot_expr :: Expr annot, dot_attribute :: Ident annot, expr_annot :: annot }
   -- | Anonymous function definition (lambda). 
   | Lambda { lambda_args :: [Parameter annot], lambda_body :: Expr annot, expr_annot :: annot }
   -- | Tuple. Can be empty. 
   | Tuple { tuple_exprs :: [Expr annot], expr_annot :: annot }
   -- | Generator yield. 
   | Yield
     -- { yield_expr :: Maybe (Expr annot) -- ^ Optional expression to yield.
     { yield_arg :: Maybe (YieldArg annot) -- ^ Optional Yield argument.
     , expr_annot :: annot
     }
   -- | Generator. 
   | Generator { gen_comprehension :: Comprehension annot, expr_annot :: annot }
   -- | Await
   | Await { await_expr :: Expr annot, expr_annot :: annot }
   -- | List comprehension. 
   | ListComp { list_comprehension :: Comprehension annot, expr_annot :: annot }
   -- | List. 
   | List { list_exprs :: [Expr annot], expr_annot :: annot }
   -- | Dictionary. 
   | Dictionary { dict_mappings :: [DictKeyDatumList annot], expr_annot :: annot }
   -- | Dictionary comprehension. /Version 3 only/. 
   | DictComp { dict_comprehension :: Comprehension annot, expr_annot :: annot }
   -- | Set. 
   | Set { set_exprs :: [Expr annot], expr_annot :: annot }
   -- | Set comprehension. /Version 3 only/. 
   | SetComp { set_comprehension :: Comprehension annot, expr_annot :: annot }
   -- | Starred expression. /Version 3 only/.
   | Starred { starred_expr :: Expr annot, expr_annot :: annot }
   -- | Parenthesised expression.
   | Paren { paren_expr :: Expr annot, expr_annot :: annot }
   -- | String conversion (backquoted expression). Version 2 only. 
   | StringConversion { backquoted_expr :: Expr annot, expr_anot :: annot }
-}
evalExpr :: forall annot. Show annot => Pc.Expr annot -> Expr
evalExpr expr =
  case expr of
    Pc.Var ident annot ->
      VarEx ident.ident_string
    Pc.Int value literal annot ->
      LiteralEx $ IntLit value
    Pc.LongInt value literal annot ->
      LiteralEx $ LIntLit value
    Pc.Float value literal annot ->
      LiteralEx $ FloatLit value
    Pc.Imaginary value literal annot ->
      LiteralEx $ FloatLit value
    Pc.Bool value annot ->
      LiteralEx $ BoolLit value
    Pc.None annot ->
      LiteralEx NoneLit
    Pc.Ellipsis annot ->
      LiteralEx EllipsisLit
    Pc.ByteStrings strings annot ->
      LiteralEx $ StringLit strings
    Pc.Strings strings annot ->
      LiteralEx $ StringLit strings
    Pc.UnicodeStrings strings annot ->
      LiteralEx $ StringLit strings
    Pc.Call { call_fun = fun, call_args = args, expr_annot = annot } ->
      CallEx (evalExpr fun) (map evalArgs args)
    Pc.Subscript { subscriptee = sub, subscript_expr = idx, expr_annot = annot } ->
      ShownEx $ "subscript: " <> show (evalExpr sub) <> " " <> show (evalExpr idx)
    Pc.SlicedExpr { slicee = slicee, slices = slices, expr_annot = annot } ->
      ShownEx $ "sliced: " <> show (evalExpr slicee) <> " " <> show slices
    Pc.CondExpr { ce_true_branch = trueBranch, ce_condition = condition, ce_false_branch = falseBranch, expr_annot = annot } ->
      CondEx (evalExpr condition) (evalExpr trueBranch) (evalExpr falseBranch)
    Pc.BinaryOp { operator = op, left_op_arg = left, right_op_arg = right, expr_annot = annot } ->
      BinOpEx (showNaryOp op) (evalExpr left) (evalExpr right)
    Pc.UnaryOp { operator = op, op_arg = arg, expr_annot = annot } ->
      UniOpEx (showNaryOp op) (evalExpr arg)
    Pc.Dot { dot_expr = expr, dot_attribute = ident, expr_annot = annot } ->
      DotEx (evalExpr expr) ident.ident_string
    Pc.Lambda { lambda_args = params, lambda_body = body, expr_annot = annot } ->
      ShownEx $ "lambda: " <> intercalate ", " (map show params) <> " " <> show body
    Pc.Tuple { tuple_exprs = exprs, expr_annot = annot } ->
      TupleEx (map evalExpr exprs)
    Pc.Yield { yield_arg = arg, expr_annot = annot } ->
      ShownEx $ "yield: " <> show arg
    Pc.Generator { gen_comprehension = comprehension, expr_annot = annot } ->
      ShownEx $ "generator: " <> show comprehension
    Pc.Await { await_expr = expr, expr_annot = annot } ->
      ShownEx $ "await: " <> show (evalExpr expr)
    Pc.ListComp { list_comprehension = comprehension, expr_annot = annot } ->
      ShownEx $ "listcomp: " <> show comprehension
    Pc.List { list_exprs = exprs, expr_annot = annot } ->
      case exprs of
        [] -> ArrayEx []
        _ -> ArrayEx (map evalExpr exprs)
    Pc.Dictionary { dict_mappings = mappings, expr_annot = annot } ->
      MapEx $ map evalDictKeyDatumList mappings
    Pc.DictComp { dict_comprehension = comprehension, expr_annot = annot } ->
      ShownEx $ "dictcomp: " <> show comprehension
    Pc.Set { set_exprs = exprs, expr_annot = annot } ->
      ShownEx $ "set: " <> intercalate ", " (map show exprs)
    Pc.SetComp { set_comprehension = comprehension, expr_annot = annot } ->
      ShownEx $ "setcomp: " <> show comprehension
    Pc.Starred { starred_expr = expr, expr_annot = annot } ->
      ShownEx $ "starred: " <> show (evalExpr expr)
    Pc.Paren { paren_expr = expr, expr_annot = annot } ->
      ParenEx (evalExpr expr)
    Pc.StringConversion { backquoted_expr = expr, expr_anot = annot } ->
      ShownEx $ "stringconversion: " <> show (evalExpr expr)

evalDictKeyDatumList :: forall annot. Show annot => Pc.DictKeyDatumList annot -> Expr
evalDictKeyDatumList list =
  case list of
    Pc.DictMappingPair key datum ->
      DictEx (evalExpr key) (evalExpr datum)
    Pc.DictUnpacking expr ->
      evalExpr expr

{-
The AST defines the operators as:
data Op annot
   = And { op_annot :: annot } -- ^ \'and\'
   | Or { op_annot :: annot } -- ^ \'or\'
   | Not { op_annot :: annot } -- ^ \'not\'
   | Exponent { op_annot :: annot } -- ^ \'**\'
   | LessThan { op_annot :: annot } -- ^ \'<\'
   | GreaterThan { op_annot :: annot } -- ^ \'>\'
   | Equality { op_annot :: annot } -- ^ \'==\'
   | GreaterThanEquals { op_annot :: annot } -- ^ \'>=\'
   | LessThanEquals { op_annot :: annot } -- ^ \'<=\'
   | NotEquals  { op_annot :: annot } -- ^ \'!=\'
   | NotEqualsV2  { op_annot :: annot } -- ^ \'<>\'. Version 2 only.
   | In { op_annot :: annot } -- ^ \'in\'
   | Is { op_annot :: annot } -- ^ \'is\'
   | IsNot { op_annot :: annot } -- ^ \'is not\'
   | NotIn { op_annot :: annot } -- ^ \'not in\'
   | BinaryOr { op_annot :: annot } -- ^ \'|\'
   | Xor { op_annot :: annot } -- ^ \'^\'
   | BinaryAnd { op_annot :: annot } -- ^ \'&\'
   | ShiftLeft { op_annot :: annot } -- ^ \'<<\'
   | ShiftRight { op_annot :: annot } -- ^ \'>>\'
   | Multiply { op_annot :: annot } -- ^ \'*\'
   | Plus { op_annot :: annot } -- ^ \'+\'
   | Minus { op_annot :: annot } -- ^ \'-\'
   | Divide { op_annot :: annot } -- ^ \'\/\'
   | FloorDivide { op_annot :: annot } -- ^ \'\/\/\'
   | MatrixMult { op_annot :: annot } -- ^ \'@\'
   | Invert { op_annot :: annot } -- ^ \'~\' (bitwise inversion of its integer argument)
   | Modulo { op_annot :: annot } -- ^ \'%\'
-}
showNaryOp :: Pc.Op annot -> String
showNaryOp op =
  case op of
    Pc.And { op_annot = annot } ->
      "and"
    Pc.Or { op_annot = annot } ->
      "or"
    Pc.Not { op_annot = annot } ->
      "not"
    Pc.Exponent { op_annot = annot } ->
      "**"
    Pc.LessThan { op_annot = annot } ->
      "<"
    Pc.GreaterThan { op_annot = annot } ->
      ">"
    Pc.Equality { op_annot = annot } ->
      "=="
    Pc.GreaterThanEquals { op_annot = annot } ->
      ">="
    Pc.LessThanEquals { op_annot = annot } ->
      "<="
    Pc.NotEquals { op_annot = annot } ->
      "!="
    Pc.NotEqualsV2 { op_annot = annot } ->
      "<>"
    Pc.In { op_annot = annot } ->
      "in"
    Pc.Is { op_annot = annot } ->
      "is"
    Pc.IsNot { op_annot = annot } ->
      "is not"
    Pc.NotIn { op_annot = annot } ->
      "not in"
    Pc.BinaryOr { op_annot = annot } ->
      "|"
    Pc.Xor { op_annot = annot } ->
      "^"
    Pc.BinaryAnd { op_annot = annot } ->
      "&"
    Pc.ShiftLeft { op_annot = annot } ->
      "<<"
    Pc.ShiftRight { op_annot = annot } ->
      ">>"
    Pc.Multiply { op_annot = annot } ->
      "*"
    Pc.Plus { op_annot = annot } ->
      "+"
    Pc.Minus { op_annot = annot } ->
      "-"
    Pc.Divide { op_annot = annot } ->
      "/"
    Pc.FloorDivide { op_annot = annot } ->
      "//"
    Pc.MatrixMult { op_annot = annot } ->
      "@"
    Pc.Invert { op_annot = annot } ->
      "~"
    Pc.Modulo { op_annot = annot } ->
      "%"


decodeTargetExpr :: forall annot. Show annot => Pc.Expr annot -> Expr
decodeTargetExpr expr =
  case expr of
    Pc.Var ident annot ->
      VarEx ident.ident_string
    _ ->
      ShownEx . show $ evalExpr expr

decodeArg :: Pc.Argument annot -> Maybe String
decodeArg arg =
  case arg of
    Pc.ArgExpr {arg_expr = e, arg_annot = a} ->
      case e of
        Pc.Var ident annot ->
          Just $ ident.ident_string
        _ -> Nothing
    Pc.ArgKeyword {arg_expr = e, arg_annot = a} ->
      case e of
        Pc.Var ident annot ->
          Just $ "meta:" <> ident.ident_string
        _ -> Nothing
    _ ->
          Nothing


showArgs :: forall annot. Show annot => [Pc.Argument annot] -> String
showArgs args =
  let
    argStrings = map showArg args
  in
  "a@(" <> intercalate "," argStrings <> ")"


showArg :: forall annot. Show annot => Pc.Argument annot -> String
showArg arg =
  case arg of
    Pc.ArgExpr {arg_expr = e, arg_annot = a} ->
      case e of
        Pc.Var ident annot ->
          ident.ident_string
        _ -> show $ evalExpr e
    Pc.ArgVarArgsPos {arg_expr = e, arg_annot = a} ->
      "a-va-p: " <> show (evalExpr e)
    Pc.ArgVarArgsKeyword {arg_expr = e, arg_annot = a} ->
      "a-va-k: " <> show (evalExpr e)
    Pc.ArgKeyword {arg_keyword = k, arg_expr = e, arg_annot = a} ->
      case e of
        Pc.Var ident annot ->
          k.ident_string <> "=" <> ident.ident_string
        _ -> k.ident_string <> "=" <> show (evalExpr e)


evalArgs :: forall annot. Show annot => Pc.Argument annot -> Argument
evalArgs arg =
  case arg of
    Pc.ArgExpr {arg_expr = e, arg_annot = a} ->
      case e of
        Pc.Var ident annot ->
          VarArg ident.ident_string
        _ -> LambdaArg $ evalExpr e
    Pc.ArgVarArgsPos {arg_expr = e, arg_annot = a} ->
      LambdaArg $ evalExpr e
    Pc.ArgVarArgsKeyword {arg_expr = e, arg_annot = a} ->
      LambdaArg $ evalExpr e
    Pc.ArgKeyword {arg_keyword = k, arg_expr = e, arg_annot = a} ->
      case e of
        Pc.Var ident annot ->
          NamedArg k.ident_string (LiteralEx $ StringLit [ident.ident_string])
        _ -> NamedArg k.ident_string (evalExpr e)
