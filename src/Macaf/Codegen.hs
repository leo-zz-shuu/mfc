{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Macaf.Codegen
    ( codegenProgram
    ) where

import           LLVM.AST                        (Operand)

import qualified LLVM.AST                        as AST
import qualified LLVM.AST.AddrSpace              as AST
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.DataLayout             as AST
import qualified LLVM.AST.Float                  as AST
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate       as IP
import           LLVM.AST.Name
import qualified LLVM.AST.Type                   as AST
import           LLVM.AST.Typed                  (typeOf)

import qualified LLVM.IRBuilder.Constant         as L
import qualified LLVM.IRBuilder.Instruction      as L
import qualified LLVM.IRBuilder.Module           as L
import qualified LLVM.IRBuilder.Monad            as L
import           LLVM.Prelude                    (ShortByteString)

import           Control.Monad.State
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import           Data.String                     (fromString)

import           Macaf.Ast

-- import Macaf.Ast (Bind(..), Op(..), Struct(..), Type(..), Uop(..))
-- import Microc.Sast
import           Macaf.Utils

import           Data.List                       (find)
import           Data.String.Conversions
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Word                       (Word32)

-- When using the IRBuilder, both functions and variables have the type Operand
data Env = Env { operands :: M.Map Text Operand
                 -- , structs :: [Struct]
               , strings  :: M.Map Text Operand
               }
  deriving (Eq, Show)

-- LLVM and Codegen type synonyms allow us to emit module definitions and basic
-- block instructions at the top level without being forced to pass explicit
-- module and builder parameters to every function
type LLVM = L.ModuleBuilderT (State Env)

type Codegen = L.IRBuilderT LLVM

registerOperand :: MonadState Env m => Text -> Operand -> m ()
registerOperand name op =
  modify $ \env -> env {operands = M.insert name op (operands env)}

instance ConvertibleStrings Text ShortByteString where
  convertString = fromString . T.unpack

ltypeOfTyp :: MonadState Env m => Type -> m AST.Type
ltypeOfTyp TyInteger = pure AST.i32

-- charStar :: AST.Type
-- charStar = AST.ptr AST.i8
-- sizeof :: MonadState Env m => Type -> m Word32
-- sizeof =
--  \case
--    TyBool -> pure 1
--    TyChar -> pure 1
--    TyInt -> pure 4
--    TyFloat -> pure 8
--    TyVoid -> pure 0
--    Pointer _ -> pure 8
--    TyStruct n -> do
--      fields <- getFields n
--      sizes <- mapM (sizeof . bindType) fields
--      pure (sum sizes)
codegenExpr :: Expr -> Codegen Operand
codegenExpr (Var e) = flip L.load 0 =<< (gets ((M.! e) . operands))
codegenExpr (Literal i) = pure $ L.int32 (fromIntegral i)
codegenExpr (Assign lhs rhs) = do
  lhs' <- codegenExpr lhs
  rhs' <- codegenExpr rhs
  L.store lhs' 0 rhs'
  return rhs'

-- codegenGlobal (MainProgramUnit mp) = do
--  typ <- ltypeOfTyp t
--  let name = mkName $ cs n
--      initVal =
--        case t of
--          Pointer _ -> C.Int 64 0
--          TyStruct _ -> C.AggregateZero typ
--          TyInt -> C.Int 32 0
--          TyBool -> C.Int 1 0
--          TyFloat -> C.Float (AST.Double 0)
--          TyChar -> C.Int 8 0
--          TyVoid -> error "Global void variables illegal"
--   var <- L.global name typ initVal
--  registerOperand n var
codegenExecutionPart :: ExecutionPart -> Codegen Operand
codegenExecutionPart (ExecutionPart ec) =
  case ec of
    Action as ->
      case as of
        Assignment e -> codegenExpr e

codegenProgramUnit :: ProgramUnit -> LLVM ()
codegenProgramUnit (MainProgramUnit mp) = codegenFunc mp

-- | Generate a function and add both the function name and variable names to
-- the map.
codegenFunc :: MainProgram -> LLVM ()
codegenFunc (MainProgram fname sp ep _)
  -- We need to forward reference the generated function and insert it into the
  -- environment _before_ generating its body in order to handle the
  -- possibility of the function calling itself recursively
 =
  mdo registerOperand (getFunctionName fname) function
  -- We wrap generating the function inside of the `locally` combinator in
  -- order to prevent local variables from escaping the scope of the function
      (function, strs) <-
        locally $
    -- retty    <- ltypeOfTyp (styp f)
    -- args     <- mapM mkParam []
         do
          fun <- L.function name [] AST.void genBody
          strings' <- gets strings
          pure (fun, strings')
      modify $ \e -> e {strings = strs}
  where
    getFunctionName (ProgramSt _fname) =
      case _fname of
        Var __fname -> __fname
    name = mkName (cs $ getFunctionName fname)
  --mkParam (Bind t n) = (,) <$> ltypeOfTyp t <*> pure (L.ParameterName (cs n))
  -- Generate the body of the function:
    getBinds :: SpecificationPart -> [Bind]
    getBinds (SpecificationPart dc) =
      case dc of
        SpecificationConstructStmt sc ->
          case sc of
            TypeDeclarationStmt td ->
              case td of
                TypeDeclaration dt ->
                  case dt of
                    IntrisicTypeSpec b -> [b]
    genBody :: [Operand] -> Codegen ()
    genBody ops = do
      _entry <- L.block `L.named` "entry"
    -- Add the formal parameters to the map, allocate them on the stack,
    -- and then emit the necessary store instructions
    -- forM_ (zip ops (sformals f)) $ \(op, Bind _ n) -> do
    --   addr <- L.alloca (typeOf op) Nothing 0
    --   L.store addr 0 op
    --   registerOperand n addr
    -- Same for the locals, except we do not emit the store instruction for
    -- them
      forM_ (getBinds sp) $ \(Bind t n) -> do
        ltype <- ltypeOfTyp t
        addr <- L.alloca ltype Nothing 0
        registerOperand n addr
    -- Evaluate the actual body of the function after making the necessary
    -- allocations
      codegenExecutionPart ep
      return ()

x86TargetTriple :: ShortByteString
x86TargetTriple = "x86_64-unknown-linux-gnu"

x86DataLayout :: AST.DataLayout
x86DataLayout =
  (AST.defaultDataLayout AST.LittleEndian)
    { AST.endianness = AST.LittleEndian
    , AST.pointerLayouts =
        M.fromList [(AST.AddrSpace 0, (64, AST.AlignmentInfo 64 64))]
    , AST.typeLayouts =
        M.fromList $
        [((AST.IntegerAlign, 1), AST.AlignmentInfo 8 8)] ++
        [((AST.IntegerAlign, w), AST.AlignmentInfo w w) | w <- [8, 16, 32, 64]] ++
        [((AST.FloatAlign, w), AST.AlignmentInfo w w) | w <- [32, 64]] ++
        [((AST.VectorAlign, w), AST.AlignmentInfo w w) | w <- [16, 32, 64, 128]]
    , AST.nativeSizes = Just $ S.fromList [16, 32, 64]
    }

codegenProgram :: Program -> AST.Module
-- codegenProgram (structs, globals, funcs) =
codegenProgram (Program progunits)
  -- flip evalState (Env {operands = M.empty, structs, strings = M.empty}) $
 =
  flip evalState (Env {operands = M.empty, strings = M.empty}) $
  fmap mkModule . L.execModuleBuilderT L.emptyModuleBuilder $
    -- L.buildModuleT "macaf" $
    -- printf <- L.externVarArgs (mkName "printf") [charStar] AST.i32
    -- registerOperand "printf" printf
    -- mapM_ emitBuiltIn builtIns
    -- mapM_ emitTypeDef structs
    -- mapM_ codegenGlobal progunit
    -- mapM_ codegenFunc funcs
  mapM_ codegenProgramUnit progunits
  where
    mkModule ds =
      AST.Module
        { AST.moduleName = "macaf"
        , AST.moduleSourceFileName = "test.macaf"
        , AST.moduleDataLayout = Just x86DataLayout
        , AST.moduleTargetTriple = Just x86TargetTriple
        , AST.moduleDefinitions = ds
        }
