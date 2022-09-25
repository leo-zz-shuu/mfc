{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Macaf.Ast where

import           Data.Char     (chr)
import           Data.Text     (Text)
import           Prettyprinter

data UnOp = Not deriving (Eq, Show)

-- Binary operators
data BinOp = Add | Sub | Mul | Div deriving (Eq, Show)

data Type = TyInteger | TyReal deriving (Eq, Show)

data Bind = Bind { bindType :: Type
                 , bindName :: Text
                 }
  deriving (Eq, Show)

data Expr = Var Text
          | Literal Integer
          | UnaryOp UnOp Expr
          | BinaryOp BinOp Expr Expr
          | Assign Expr Expr
  deriving (Eq, Show)

data ProgramName = ProgramName { programName :: Maybe Text
                               }
  deriving (Eq, Show)

-- data Contains_stmt = CONTAINS

-- data AssignmentStmt =
--   AssignmentStmt Expr
data ActionStmt = Assignment Expr
  deriving (Eq, Show)

data ExecutableConstruct = Action ActionStmt
  deriving (Eq, Show)

data ExecutionPart = ExecutionPart ExecutableConstruct
  deriving (Eq, Show)

data DeclarationTypeSpec = IntrisicTypeSpec Bind
  deriving (Eq, Show)

data TypeDeclaration = TypeDeclaration DeclarationTypeSpec
  deriving (Eq, Show)

data SpecificationConstruct = TypeDeclarationStmt TypeDeclaration
  deriving (Eq, Show)

data DeclarationConstruct = SpecificationConstructStmt SpecificationConstruct
  deriving (Eq, Show)

data SpecificationPart = SpecificationPart DeclarationConstruct
  deriving (Eq, Show)

-- data Function_stmt =
--   FUNCTION FunctionName
-- data EndFunctionStmt =
--   END [FUNCTION [FunctionName]]

type FunctionName = Text
type SubprogramName = Text

data SubprogramPart = FunctionPart FunctionName [SpecificationPart] [ExecutionPart] [InternalSubprogramPart]
                    | SubroutinePart SubprogramName [SpecificationPart] [ExecutionPart] [InternalSubprogramPart]
  deriving (Eq, Show)

data InternalSubprogramPart = Contains [SubprogramPart]
  deriving (Eq, Show)

data MainProgram = MainProgram ProgramName [SpecificationPart] [ExecutionPart] [InternalSubprogramPart]
  deriving (Eq, Show)

data ProgramUnit = MainProgramUnit MainProgram
                 | FunctionUnit
                 | SubroutineUnit
                 | ModuleUnit
                 | SubModuleUnit
                 | BlockDataUnit
  deriving (Eq, Show)

data Program = Program [ProgramUnit]
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
      Var name            -> pretty name
      Literal l           -> pretty l
      UnaryOp op e        -> pretty op <> parens (pretty e)
      BinaryOp op lhs rhs -> hsep [pretty lhs, pretty op, pretty rhs]
      Assign lhs rhs      -> pretty lhs <+> "=" <+> pretty rhs
