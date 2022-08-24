{-# LANGUAGE OverloadedStrings #-}

module Macaf.Parser.Combinator
  ( programP
  , runParser
  , errorBundlePretty
  ) where

import Control.Applicative (liftA2, liftA3)

import Control.Monad.Combinators.Expr
import Data.Either
import Macaf.Ast
import Macaf.Scanner.Combinator
import Text.Megaparsec

opTable :: [[Operator MacafParser Expr]]
opTable =
  [ [infixL Mul "*", infixL Div "/"]
  , [infixL Add "+", infixL Sub "-"]
  , [InfixR $ Assign <$ symbol "="]
  ]
  -- Megaparsec doesn't support multiple prefix operators by default,
  -- but we need this in order to parse things like double negatives,
  -- nots, and dereferences
    -- unary op sym = Prefix $ foldr1 (.) <$> some (op <$ symbol sym)
  where
    infixL op sym = InfixL $ BinaryOp op <$ symbol sym
  -- Primed infixL' is useful for operators which are prefixes of other operators
    -- infixL' op sym = InfixL $ Binop op <$ operator sym
    -- infixR op sym = InfixR $ Binop op <$ symbol sym
    -- operator sym = lexeme $ try (symbol sym <* notFollowedBy opChar)
    -- opChar = oneOf ("!#$%&*+./<=>?@\\^|-~" :: String)

integerP :: MacafParser Expr
integerP = Literal <$> integer

variableP :: MacafParser Expr
variableP = Var <$> identifier

termP :: MacafParser Expr
termP = choice [parens exprP, variableP, integerP]

--  parens exprP <|>
--  Null <$ pKeyword "NULL" <|>
--  try (Fliteral <$> float) <|>
--  Literal <$> int <|>
--  BoolLit <$> (True <$ pKeyword "true" <|> False <$ pKeyword "false") <|>
--  Sizeof <$> (pKeyword "sizeof" *> parens typeP) <|>
--  try (Call <$> identifier <*> parens (exprP `sepBy` comma)) <|>
--  CharLit <$> charlit <|>
--  StrLit <$> strlit <|>
--  Id <$> identifier
exprP :: MacafParser Expr
exprP = makeExprParser termP opTable

typeP :: MacafParser Type
typeP = TyInteger <$ pKeyword "integer" <|> TyReal <$ pKeyword "real"
  -- baseType <-
  --  TyInteger <$ pKeyword "integer" <|> TyReal <$ pKeyword "real"
  -- foldr (const Pointer) baseType <$> many star

vdeclP :: MacafParser Bind
vdeclP = Bind <$> typeP <*> identifier

programStmtP :: MacafParser ProgramStmt
programStmtP = ProgramSt <$> (pKeyword "program" *> variableP)

actionStmtP :: MacafParser ActionStmt
actionStmtP = Assignment <$> exprP

executableConstructP :: MacafParser ExecutableConstruct
executableConstructP = Action <$> actionStmtP

executionPartP :: MacafParser ExecutionPart
executionPartP = ExecutionPart <$> executableConstructP

endProgramStmtP :: MacafParser EndProgramStmt
endProgramStmtP =
  EndProgramStmt <$> (pKeyword "end" *> pKeyword "program" *> variableP)

declarationTypeSpecP :: MacafParser DeclarationTypeSpec
declarationTypeSpecP = IntrisicTypeSpec <$> vdeclP

typeDeclarationP :: MacafParser TypeDeclaration
typeDeclarationP = TypeDeclaration <$> declarationTypeSpecP

specificationConstructP :: MacafParser SpecificationConstruct
specificationConstructP = TypeDeclarationStmt <$> typeDeclarationP

declarationConstructP :: MacafParser DeclarationConstruct
declarationConstructP = SpecificationConstructStmt <$> specificationConstructP

specificationPartP :: MacafParser SpecificationPart
specificationPartP = SpecificationPart <$> declarationConstructP

mainProgramP :: MacafParser MainProgram
mainProgramP =
  MainProgram <$> programStmtP <*> specificationPartP <*> executionPartP <*>
  endProgramStmtP

programUnitP :: MacafParser ProgramUnit
programUnitP = MainProgramUnit <$> mainProgramP

programP :: MacafParser Program
programP = Program <$> many programUnitP
