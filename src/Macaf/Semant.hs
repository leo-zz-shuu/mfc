{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Macaf.Semant
    ( checkProgram
    ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.List            (find, findIndex)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust, isJust, isNothing)
import           Data.Text            (Text)
import           Macaf.Ast
import           Macaf.Sast

-- import Macaf.Semant.Analysis
import           Macaf.MessageType
import           Macaf.Utils

-- type Vaars = M.Map (Text, VarKind) Type
-- type Funcs = M.Map Text Function
-- type Structs = [Struct]
data Env = Env { mainprog :: Maybe MainProgram
               }

type Semant = ExceptT Msg (State Env)

checkProgUnit :: ProgramUnit -> Semant SProgramUnit
checkProgUnit (MainProgramUnit mainp) = do
  mp <- gets mainprog
  unless (isNothing mp) $ throwError $ ErrorKind E_206001
  modify $ \env -> env {mainprog = Just mainp}
  pure $ MainProgramUnit mainp

checkProgram :: Program -> Either Msg SProgram
checkProgram program = evalState (runExceptT (checkProgram' program)) baseEnv
  where
    baseEnv = Env {mainprog = Nothing}
    checkProgram' :: Program -> Semant SProgram
    checkProgram' (Program progunits) = do
      pgs <- mapM checkProgUnit progunits
      pure $ program
