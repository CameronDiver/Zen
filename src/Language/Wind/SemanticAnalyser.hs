module Language.Wind.SemanticAnalyser
  ( checkProgram
  , Language.Wind.SemanticAnalyser.Error.SemanticError
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.List                            (find)
import qualified Data.Map                             as M
import           Data.Maybe                           (fromMaybe, isJust,
                                                       isNothing)
import           Data.Text                            (Text)
import           Debug.Trace                          (traceShow)

import           Language.Wind.AST
import           Language.Wind.SemanticAnalyser.AST
import           Language.Wind.SemanticAnalyser.Error

-- TODO: Also store whether these values are const
type Vars = M.Map (Text, VarScope) Type

type Functions = M.Map Text Function

data Env
  = Env
      { vars :: Vars
      , functions :: Functions
      }

type Semantic = ExceptT SemanticError (State Env)

checkProgram :: Program -> Either SemanticError SAProgram
checkProgram program = evalState (runExceptT (checkProgram' program)) baseEnv
  where
    baseEnv = Env {vars = M.empty, functions = builtInFunctions}
    checkProgram' (Program statements) = do
      stmts <- mapM checkStatement statements
      pure $ SAProgram stmts

checkExpr :: Expr -> Semantic SAExpr
checkExpr expr =
  case expr of
    Literal i -> pure (TyInt, SALiteral i)
    StringLiteral t -> pure (TyString, SAStringLiteral t)
    CharLiteral c -> pure (TyChar, SACharLiteral c)
    FloatLiteral f -> pure (TyDouble, SAFloatLiteral f)
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
    -- Special case, we don't yet have variadic functions,
    -- so let this through
    Call "printf" vals -> do
      when (length vals == 0) $
        throwError $ InvalidArgumentCount 1 (length vals)
      args <- mapM checkExpr vals
      let fmt = head args
      when (fst fmt /= TyString) $ throwError $ TypeError [TyString] (fst fmt)
      pure (TyVoid, SACall "printf" args)
    Call name vals
      -- Check that a function exists with the given name
     -> do
      fns <- gets functions
      let maybeFn = M.lookup name fns
      when (isNothing maybeFn) $ throwError $ UndefinedSymbol name
      let (Just fn) = maybeFn
      args <- mapM checkExpr vals
      -- Ensure the expressions provided match the parameter
      -- types, and we have the correct amount
      when (length args /= length (params fn)) $
        throwError $
        InvalidArgumentCount
          {required = length args, provided = length $ params fn}
      forM_ (zip (map fst args) (map fst $ params fn)) $ \(t1, t2) ->
        unless (t1 == t2) $ throwError $ TypeError {expected = [t2], got = t1}
      pure (returnType fn, SACall name args)
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
            (TyInt, TyDouble) -> pure (TyDouble, sexpr)
            (TyDouble, TyInt) -> pure (TyDouble, sexpr)
            (TyDouble, TyDouble) -> pure (TyDouble, sexpr)
            (TyInt, TyChar) -> pure (TyChar, sexpr)
            (TyChar, TyInt) -> pure (TyChar, sexpr)
            -- FIXME: This error is not true in all cases!
            _ -> throwError $ TypeError [TyInt, TyChar, TyDouble] t1
    Sub ->
      let sexpr = SABinaryOp op lhs' rhs'
       in case (t1, t2) of
            (TyInt, TyInt) -> pure (TyInt, sexpr)
            (TyInt, TyDouble) -> pure (TyDouble, sexpr)
            (TyDouble, TyDouble) -> pure (TyDouble, sexpr)
            (TyChar, TyInt) -> pure (TyChar, sexpr)
            (TyInt, TyChar) -> pure (TyChar, sexpr)
            -- FIXME: This error is not true in all cases!
            _ -> throwError $ TypeError [TyInt, TyChar, TyDouble] t1
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

builtInFunctions :: Functions
builtInFunctions =
  M.fromList $ map createFn [("printf", TyVoid, [TyString, TyInt])]
  where
    createFn (name, ret, params) =
      (name, Function ret name $ map createParam params)
    createParam t = (t, SAIdentifier "dummyVar")
