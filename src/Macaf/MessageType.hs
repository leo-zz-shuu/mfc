{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Macaf.MessageType where

import           Data.Text     (Text)
import           Macaf.Ast
import           Prettyprinter

type Name = Text

data BindingKind = Duplicate | Void deriving (Show)

instance Pretty BindingKind where
  pretty = unsafeViaShow

data SymbolKind = VarSymbol | FuncSymbol deriving (Show)

instance Pretty SymbolKind where
  pretty =
    \case
      VarSymbol  -> "variable"
      FuncSymbol -> "function"

data VarKind = Global | Local deriving (Eq, Ord, Show)

instance Pretty VarKind where
  pretty = unsafeViaShow

data SemantError = UndefinedSymbol Name SymbolKind Expr
                 | Redeclaration Name
                 | AssignmentError { _lhs :: Expr
                                   , _rhs :: Expr
                                   }
  deriving (Show)

data InfoMsg -- 000001 ~ 100000: General Message
             = I_000001
             -- 100001 - 200000: Parse Message
             | I_100001 Text
             -- 200001 - 300000: Semantic Message
             | I_200001
             -- 300001 - 400000: Codegen Message
             | I_300001
  deriving (Show)

instance Pretty InfoMsg where
  pretty =
    \case
      I_000001 -> "Infomation"

data WarningMsg -- 002001 ~ 100000: General Message
                = W_002001
                -- 102001 - 200000: Parse Message
                | W_102001
                -- 202001 - 300000: Semantic Message
                | W_202001 Text
                -- 302001 - 400000: Codegen Message
                | W_302001
  deriving (Show)

instance Pretty WarningMsg where
  pretty =
    \case
      W_002001 -> "Warning"

instance Pretty SemantError where
  pretty =
    \case
      UndefinedSymbol name symKind expr -> "UndefinedSymbol"
      Redeclaration name -> "redeclaration of function" <+> pretty name
      AssignmentError lhs rhs ->
        "Cannot assign" <+> pretty rhs <+> "to" <+> pretty lhs

data ErrorMsg -- 006001 ~ 100000: General Message
              = E_006001
              -- 106001 - 200000: Parse Message
              | E_106001 Text
              -- 206001 - 300000: Semantic Message
              | E_206001
              -- 306001 - 400000: Codegen Message
              | E_306001
  deriving (Show)

instance Pretty ErrorMsg where
  pretty =
    \case
      E_206001 -> "The Main Program should be declared once in the program"

data Msg = InfoKind InfoMsg
         | WarningKind WarningMsg
         | ErrorKind ErrorMsg
  deriving (Show)

instance Pretty Msg where
  pretty =
    \case
      InfoKind msg    -> "[Info Message:]" <+> pretty msg
      WarningKind msg -> "[Warning Message:]" <+> pretty msg
      ErrorKind msg   -> "[Error Message:]" <+> pretty msg
