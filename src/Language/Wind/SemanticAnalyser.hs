module Language.Wind.SemanticAnalyser
  ( checkProgram
  , Language.Wind.SemanticAnalyser.Error.SemanticError
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.List                            (find)
import qualified Data.Map                             as M
import           Data.Maybe                           (fromMaybe)
import           Data.Maybe                           (isJust)
import           Data.Text                            (Text)

import           Language.Wind.AST
import           Language.Wind.SemanticAnalyser.AST
import           Language.Wind.SemanticAnalyser.Error

-- TODO: Also store whether these values are const
type Vars = M.Map (Text, VarScope) Type

data Env =
  Env
    { vars :: Vars
    }

type Semantic = ExceptT SemanticError (State Env)

checkProgram :: Program -> Either SemanticError SAProgram
checkProgram program = evalState (runExceptT (checkProgram' program)) baseEnv
  where
    baseEnv = Env {vars = M.empty}
    checkProgram' (Program statements) = do
      stmts <- mapM checkStatement statements
      pure $ SAProgram stmts

checkExpr :: Expr -> Semantic SAExpr
checkExpr expr =
  case expr of
    Literal i -> pure (TyInt, SALiteral i)
    StringLiteral t -> pure (TyString, SAStringLiteral t)
    CharLiteral c -> pure (TyChar, SACharLiteral c)
    -- For variables, let's ensure that it has been created
    -- before we reference it
    Identifier sym -> do
      vars <- gets vars
      let foundVars = map (\kind -> M.lookup (sym, kind) vars) [Local, Global]
      case join $ find isJust foundVars of
        Nothing -> throwError $ UndefinedSymbol sym
        Just ty -> pure (ty, SAIdentifier sym)
    b@(BinaryOp _ _ _) -> checkBinaryOp b
    Assign lhs rhs -> do
      lhs'@(t1, _) <- checkExpr lhs
      rhs'@(t2, _) <- checkExpr rhs
      -- Check the lhs is something we cna assign to
      case snd lhs' of
        SAIdentifier _ -> do
          assertTypeMatch t1 t2
          pure (t2, SAAssign lhs' rhs')
        _ -> throwError $ InvalidAssignmentLval lhs
    VarDeclaration target ->
      case target of
        (Identifier name) -> do
          tryCreateVar name Nothing
          pure (TyFlexible, SAVarDeclaration (TyFlexible, SAIdentifier name))
        (Assign v e)
          -- Ensure that we're assigning to an identifier
         ->
          case v of
            Identifier name -> do
              e'@(eType, _) <- checkExpr e
              tryCreateVar name $ Just eType
              pure (eType, SAVarInitialize (eType, SAIdentifier name) e')
            _ -> throwError $ InvalidVarDeclaration target
        _ -> throwError $ InvalidVarDeclaration target
    NoExpr -> pure (TyVoid, SANoExpr)

checkBinaryOp :: Expr -> Semantic SAExpr
checkBinaryOp (BinaryOp op rhs lhs) = do
  lhs'@(t1, _) <- checkExpr lhs
  rhs'@(t2, _) <- checkExpr rhs
  case op of
    Add ->
      let sexpr = SABinaryOp op lhs' rhs'
       in case (t1, t2) of
            (TyInt, TyInt) -> pure (TyInt, sexpr)
            (TyInt, TyFloat) -> pure (TyFloat, sexpr)
            (TyFloat, TyInt) -> pure (TyFloat, sexpr)
            (TyFloat, TyFloat) -> pure (TyFloat, sexpr)
            (TyInt, TyChar) -> pure (TyChar, sexpr)
            (TyChar, TyInt) -> pure (TyChar, sexpr)
            -- FIXME: This error is not true in all cases!
            _ -> throwError $ TypeError [TyInt, TyChar, TyFloat] t1
    Sub ->
      let sexpr = SABinaryOp op lhs' rhs'
       in case (t1, t2) of
            (TyInt, TyInt) -> pure (TyInt, sexpr)
            (TyInt, TyFloat) -> pure (TyFloat, sexpr)
            (TyFloat, TyFloat) -> pure (TyFloat, sexpr)
            (TyChar, TyInt) -> pure (TyChar, sexpr)
            (TyInt, TyChar) -> pure (TyChar, sexpr)
            -- FIXME: This error is not true in all cases!
            _ -> throwError $ TypeError [TyInt, TyChar, TyFloat] t1
-- TODO: Throw an error
checkBinaryOp _ = undefined

checkStatement :: Statement -> Semantic SAStatement
checkStatement (Expr e) = SAExpr <$> checkExpr e

assertTypeMatch :: Type -> Type -> Semantic ()
assertTypeMatch t1 t2 =
  unless (t1 == t2 || t1 == TyFlexible) $ throwError $ TypeError [t1] t2

tryCreateVar :: Text -> Maybe Type -> Semantic ()
tryCreateVar name t = do
  vars <- gets vars
  -- Add this to the variables
  -- FIXME: Scope is wrong, assuming all
  -- global for now
  when (M.member (name, Global) vars) $
    throwError $ DuplicateVarDeclaration name
  modify $ \env -> env {vars = M.insert (name, Global) varType vars}
  where
    varType = fromMaybe TyFlexible t
