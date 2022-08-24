{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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

data ProgramUnit
  = MainProgramUnit MainProgram
  | FunctionUnit
  | SubroutineUnit
  | ModuleUnit
  | SubModuleUnit
  | BlockDataUnit
  deriving (Eq, Show)

data Program =
  Program [ProgramUnit]
  deriving (Eq, Show)

--------------------------------------------
-- Pretty instances
--------------------------------------------
instance Pretty UnOp where
  pretty =
    \case
      Not -> "!"

instance Pretty BinOp where
  pretty =
    \case
      Add -> "+"
      Sub -> "-"
      Mul -> "*"
      Div -> "/"

instance Pretty Expr where
  pretty =
    \case
      Var name -> pretty name
      Literal l -> pretty l
      UnaryOp op e -> pretty op <> parens (pretty e)
      BinaryOp op lhs rhs -> hsep [pretty lhs, pretty op, pretty rhs]
      Assign lhs rhs -> pretty lhs <+> "=" <+> pretty rhs
