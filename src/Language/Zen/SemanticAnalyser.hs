module Language.Zen.SemanticAnalyser
  ( checkProgram
  , Language.Zen.SemanticAnalyser.Error.SemanticError
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.List                           (find)
import qualified Data.Map                            as M
import           Data.Maybe                          (fromMaybe, isJust,
                                                      isNothing)
import           Data.Text                           (Text)

import           Language.Zen.AST
import           Language.Zen.SemanticAnalyser.AST
import           Language.Zen.SemanticAnalyser.Error

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
    Literal _ i -> pure (TyInt, SALiteral i)
    StringLiteral _ t -> pure (TyString, SAStringLiteral t)
    CharLiteral _ c -> pure (TyChar, SACharLiteral c)
    FloatLiteral _ f -> pure (TyDouble, SAFloatLiteral f)
    BooleanLiteral _ b -> pure (TyBoolean, SABooleanLiteral b)
    -- For variables, let's ensure that it has been created
    -- before we reference it
    Identifier loc sym -> do
      vars <- gets vars
      let foundVars = fmap (\kind -> M.lookup (sym, kind) vars) [Local, Global]
      case join $ find isJust foundVars of
        Nothing -> throwError $ UndefinedSymbol loc sym
        Just ty -> pure (ty, SAIdentifier sym)
    b@(BinaryOp _ _ _ _) -> checkBinaryOp b
    Assign _ lhs rhs -> do
      lhs'@(t1, _) <- checkExpr lhs
      rhs'@(t2, _) <- checkExpr rhs
      -- Check the lhs is something we cna assign to
      case snd lhs' of
        SAIdentifier _ -> do
          assertTypeMatch (exprLoc rhs) t1 t2
          pure (t2, SAAssign lhs' rhs')
        _ -> throwError $ InvalidAssignmentLval (exprLoc lhs) lhs
    VarDeclaration loc target ->
      case target of
        (Identifier _ name) -> do
          tryCreateVar loc name Nothing
          pure (TyFlexible, SAVarDeclaration (TyFlexible, SAIdentifier name))
        (Assign l' v e)
          -- Ensure that we're assigning to an identifier
         ->
          case v of
            Identifier _ name -> do
              e'@(eType, _) <- checkExpr e
              tryCreateVar loc name $ Just eType
              pure (eType, SAVarInitialize (eType, SAIdentifier name) e')
            _ -> throwError $ InvalidVarDeclaration l' target
        _ -> throwError $ InvalidVarDeclaration loc target
    -- Special case, we don't yet have variadic functions,
    -- so let this through
    Call loc "printf" vals -> do
      when (null vals) $ throwError $ InvalidArgumentCount loc 1 (length vals)
      args <- mapM checkExpr vals
      let fmt = head args
      when (fst fmt /= TyString) $
        throwError $ TypeError loc [TyString] (fst fmt)
      pure (TyVoid, SACall "printf" args)
    Call loc name vals
      -- Check that a function exists with the given name
     -> do
      fns <- gets functions
      let maybeFn = M.lookup name fns
      when (isNothing maybeFn) $ throwError $ UndefinedSymbol loc name
      let (Just fn) = maybeFn
      args <- mapM checkExpr vals
      -- Ensure the expressions provided match the parameter
      -- types, and we have the correct amount
      when (length args /= length (params fn)) $
        throwError $ InvalidArgumentCount loc (length args) (length $ params fn)
      forM_ (zip (fmap fst args) (fst <$> params fn)) $ \(t1, t2) ->
        unless (t1 == t2) $ throwError $ TypeError loc [t2] t1
      pure (returnType fn, SACall name args)
    NoExpr -> pure (TyVoid, SANoExpr)

checkBinaryOp :: Expr -> Semantic SAExpr
checkBinaryOp (BinaryOp loc op lhs rhs) = do
  lhs'@(t1, _) <- checkExpr lhs
  rhs'@(t2, _) <- checkExpr rhs
  let sexpr = SABinaryOp op lhs' rhs' in case op of
    Add ->
       case (t1, t2) of
            (TyInt, TyInt) -> pure (TyInt, sexpr)
            (TyInt, TyDouble) -> pure (TyDouble, sexpr)
            (TyDouble, TyInt) -> pure (TyDouble, sexpr)
            (TyDouble, TyDouble) -> pure (TyDouble, sexpr)
            (TyInt, TyChar) -> pure (TyChar, sexpr)
            (TyChar, TyInt) -> pure (TyChar, sexpr)
            -- FIXME: This error is not true in all cases!
            _ -> throwError $ TypeError loc [TyInt, TyChar, TyDouble] t1
    Sub ->
      case (t1, t2) of
            (TyInt, TyInt) -> pure (TyInt, sexpr)
            (TyInt, TyDouble) -> pure (TyDouble, sexpr)
            (TyDouble, TyDouble) -> pure (TyDouble, sexpr)
            (TyDouble, TyInt) -> pure (TyDouble, sexpr)
            (TyChar, TyInt) -> pure (TyChar, sexpr)
            (TyInt, TyChar) -> pure (TyChar, sexpr)
            -- FIXME: This error is not true in all cases!
            _ -> throwError $ TypeError loc [TyInt, TyChar, TyDouble] t1
    -- No TyChar cases here because I think it's time to get
    -- rid of the char type
    Mul ->
      case (t1, t2) of
        (TyInt, TyInt) -> pure (TyInt, sexpr)
        (TyDouble, TyDouble) -> pure (TyDouble, sexpr)
        (TyInt, TyDouble) -> pure (TyDouble, sexpr)
        (TyDouble, TyInt) -> pure (TyDouble, sexpr)
        _ -> throwError $ TypeError loc [TyInt, TyDouble] t1
    Div ->
      case (t1, t2) of
        (TyInt, TyInt)       -> pure (TyInt, sexpr)
        (TyDouble, TyDouble) -> pure (TyDouble, sexpr)
        (TyInt, TyDouble)    -> pure (TyDouble, sexpr)
        (TyDouble, TyInt) -> pure (TyDouble, sexpr)
        _ -> throwError $ TypeError loc [TyInt, TyDouble] t1

-- TODO: Throw an error
checkBinaryOp _ = undefined

checkStatement :: Statement -> Semantic SAStatement
checkStatement (Expr e) = SAExpr <$> checkExpr e
checkStatement (If loc predicate iBody eBody) = do
  p <- checkExpr predicate
  -- Make sure we're getting a boolean from the predicate
  -- Add three to the location as it skips the "if " and
  -- points at the predicate
  assertTypeMatch (loc {locColumn = locColumn loc + 3}) TyBoolean (fst p)
  checkedIfBody <- mapM checkStatement iBody
  checkedElseBody <- mapM checkStatement eBody
  pure (SAIfStatement p checkedIfBody checkedElseBody)
checkStatement (While loc predicate body) = do
  p <- checkExpr predicate
  assertTypeMatch (loc {locColumn = locColumn loc + 6}) TyBoolean (fst p)

  checkedBody <- mapM checkStatement body
  pure $ SAWhileStatement p checkedBody

assertTypeMatch :: Location -> Type -> Type -> Semantic ()
assertTypeMatch l t1 t2 =
  unless (t1 == t2 || t1 == TyFlexible) $ throwError $ TypeError l [t1] t2

tryCreateVar :: Location -> Text -> Maybe Type -> Semantic ()
tryCreateVar l name t = do
  vars <- gets vars
  -- Add this to the variables
  -- FIXME: Scope is wrong, assuming all
  -- global for now
  when (M.member (name, Global) vars) $
    throwError $ DuplicateVarDeclaration l name
  modify $ \env -> env {vars = M.insert (name, Global) varType vars}
  where
    varType = fromMaybe TyFlexible t

builtInFunctions :: Functions
builtInFunctions =
  M.fromList $ fmap createFn [("printf", TyVoid, [TyString, TyInt])]
  where
    createFn (name, ret, params) =
      (name, Function ret name $ fmap createParam params)
    createParam t = (t, SAIdentifier "dummyVar")
