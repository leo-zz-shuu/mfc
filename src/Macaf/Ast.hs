module Macaf.Ast where

import Data.Char (chr)
import Data.Text (Text)
import Prettyprinter

data UnOp =
  Not
  deriving (Show, Eq)

-- Binary operators
data BinOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq)

data Type
  = TyInteger
  | TyReal
  deriving (Show, Eq)

data Bind =
  Bind
    { bindType :: Type
    , bindName :: Text
    }
  deriving (Show, Eq)

data Expr
  = Var Text
  | Literal Integer
  | UnaryOp UnOp Expr
  | BinaryOp BinOp Expr Expr
  | Assign Expr Expr
  deriving (Eq, Show)

type ProgramName = Expr

data ProgramStmt =
  ProgramSt ProgramName
  deriving (Eq, Show)

-- data Contains_stmt = CONTAINS
data EndProgramStmt =
  EndProgramStmt ProgramName
  deriving (Eq, Show)

-- data AssignmentStmt =
--   AssignmentStmt Expr
data ActionStmt =
  Assignment Expr
  deriving (Eq, Show)

data ExecutableConstruct =
  Action ActionStmt
  deriving (Eq, Show)

data ExecutionPart =
  ExecutionPart ExecutableConstruct
  deriving (Eq, Show)

data DeclarationTypeSpec =
  IntrisicTypeSpec Bind
  deriving (Eq, Show)

data TypeDeclaration =
  TypeDeclaration DeclarationTypeSpec
  deriving (Eq, Show)

data SpecificationConstruct =
  TypeDeclarationStmt TypeDeclaration
  deriving (Eq, Show)

data DeclarationConstruct =
  SpecificationConstructStmt SpecificationConstruct
  deriving (Eq, Show)

data SpecificationPart =
  SpecificationPart DeclarationConstruct
  deriving (Eq, Show)

-- data Function_stmt =
--   FUNCTION FunctionName
-- data EndFunctionStmt =
--   END [FUNCTION [FunctionName]]
-- data FunctionSubprogram = Function_stmt End_function_stmt
-- data Internal_subprogram = Function_subprogram
-- data Internal_subprogram_part = Contains_stmt [Internal_subprogram]
data MainProgram =
  MainProgram
    ProgramStmt
    SpecificationPart
    ExecutionPart
    -- InternalSubprogramPart
    EndProgramStmt
  deriving (Eq, Show)

data ProgramUnit =
  MainProgramUnit MainProgram
  deriving (Eq, Show)

--  | External_subprogram
--  | Module
--  | SubModule
--  | Block_Data
data Program =
  Program ProgramUnit
  deriving (Eq, Show) --------------------------------------------
-- Pretty instances
--------------------------------------------
-- instance Pretty Function where
--   pretty (Function typ name formals locals body) =
--     pretty typ <+>
--     pretty name <>
--     tupled (map pretty formals) <>
--     hardline <>
--     lbrace <>
--     hardline <>
--     indent 4 (hardsep (map decl locals ++ map pretty body)) <>
--     hardline <> rbrace <> hardline
-- instance Pretty Program where
--   pretty (Program unit) = hardsep (map pretty unit)
-- decl :: Pretty a => a -> Doc ann
-- decl bind = pretty bind <> semi
-- | Separates many docs with hardlines
-- hardsep :: [Doc ann] -> Doc ann
-- hardsep = concatWith (\x y -> x <> hardline <> y)
