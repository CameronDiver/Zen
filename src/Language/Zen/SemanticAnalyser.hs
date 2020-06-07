module Language.Zen.SemanticAnalyser
  ( checkProgram
  , Language.Zen.SemanticAnalyser.Error.SemanticError
  ) where

import           Control.Monad                       (when)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.List                           (find, partition)
import qualified Data.Map                            as M
import           Data.Maybe                          (fromJust, fromMaybe,
                                                      isJust, isNothing)
import           Data.Text                           (Text)

import           Language.Zen.AST                    as AST
import           Language.Zen.SemanticAnalyser.AST   as SAST
import           Language.Zen.SemanticAnalyser.Env
import           Language.Zen.SemanticAnalyser.Error
import           Language.Zen.SemanticAnalyser.Scope
import           Language.Zen.SemanticAnalyser.Types

checkProgram :: Program -> Either SemanticError SAProgram
checkProgram program = evalState (runExceptT (checkProgram' program)) baseEnv
  where
    baseEnv =
      Env
        { vars = newScopeStack
        , functions = builtInFunctions
        -- This will be overwritten as soon as we get to the
        -- first function, but I don't want this to be a
        -- Maybe and have to constantly check
        , currentFunction = FunctionInterface "dummy" TyVoid []
        , structs = []
        }
    checkProgram' :: Program -> Semantic SAProgram
    checkProgram' (Program entries)
      -- We want to create an implicit main function if any
      -- of the statements are top level
      -- FIXME: Disallow top level statements if main is
      -- defined
     = do
      let (ss, other) = partition isStatement entries
      let (fns, structs) = partition isFunction other
      let allFns =
            fmap
              (\(AST.Fn s) -> s)
              (if not $ null ss
                -- Add main to the end, as currently
                -- functions must have been generated before
                -- they can be used
                 then fns <> [generateMain ss]
                 else fns)
      mapM_ addFunctionInterface allFns
      mapM_ addStructs $ fmap (\(AST.Struct s) -> s) structs
      checked <- mapM checkFunction allFns
      pure $ SAProgram checked
    isStatement topLevel =
      case topLevel of
        (AST.Statement _) -> True
        _                 -> False
    isFunction topLevel =
      case topLevel of
        (AST.Struct _) -> True
        _              -> False
    -- TODO: Change this janky code (we add a return 0 at
    -- the end when the user is not implementing main themselves)
    generateMain stmts =
      AST.Fn $
      FunctionDef
        loc
        "main"
        []
        "int"
        loc
        (fmap (\(AST.Statement s) -> s) stmts <>
         [Expr $ Return loc (Just (Literal loc 0))])
    loc = Location 0 "" 0

addFunctionInterface :: FunctionDef -> Semantic ()
addFunctionInterface (FunctionDef _ name args ret retloc _) = do
  when (isJust unknownType) $ throwError $ UndefinedType argLoc argType
  when (isNothing returnType) $ throwError $ UndefinedType retloc ret
  functions <- gets functions
  modify
    (\env ->
       env
         { functions =
             M.insert
               name
               (FunctionInterface
                  name
                  (fromJust returnType)
                  (certainArgTypes argTypes))
               functions
         })
  where
    argTypes =
      zip args $ fmap (\FunctionArg {argType = t} -> typeFromText t) args
    unknownType = find (\(_, t) -> isNothing t) argTypes
    certainArgTypes = fmap (\(FunctionArg {name = n}, t) -> (fromJust t, n))
    (FunctionArg argLoc _ argType) = fst $ fromJust unknownType
    returnType = typeFromText ret

addStructs :: StructDef -> Semantic ()
addStructs (StructDef loc name fields)
  -- TODO: Check that all of the fields are of the
  -- same name
 = do
  struct <- SAStruct name <$> mapM checkStructField fields
  modify (\env -> env {structs = struct : structs env})
  where
    checkStructField (StructMember _ public fieldName ty) = do
      let t = typeFromText ty
      when (isNothing t) $ throwError $ UndefinedType loc ty
      pure $ SAStructMember public fieldName (fromJust t)
    checkStructField (StructFn _ public fn) =
      SAStructFunction public <$> checkFunction fn

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
      let foundVars = findVariable vars sym
      case foundVars of
        Nothing      -> throwError $ UndefinedSymbol loc sym
        Just (ty, _) -> pure (ty, SAIdentifier sym)
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
    Return loc retExpr -> do
      curr <- gets currentFunction
      let currFnReturnType = returnType curr
      value <- maybe (pure Nothing) (fmap Just . checkExpr) retExpr
      case value of
        Just (t, _) -> do
          unless (t == currFnReturnType) $
            throwError $ TypeError loc [currFnReturnType] t
          pure (currFnReturnType, SAReturn value)
        Nothing -> do
          unless (currFnReturnType == TyVoid) $
            throwError $ TypeError loc [currFnReturnType] TyVoid
          pure (TyVoid, SAReturn Nothing)
    Index loc source idx -> do
      sourceE <- checkExpr source
      idxE <- checkExpr idx
      unless (isWrappedType (fst sourceE)) $
        throwError $ ExpectedWrappedTypeError loc (fst sourceE)
      -- FIXME: Check for int idx
      pure (unwrapType (fst sourceE), SAIndex sourceE idxE)
    NoExpr -> pure (TyVoid, SANoExpr)

checkBinaryOp :: Expr -> Semantic SAExpr
checkBinaryOp (BinaryOp loc op lhs rhs) = do
  lhs'@(t1, _) <- checkExpr lhs
  rhs'@(t2, _) <- checkExpr rhs
  let sexpr = SABinaryOp op lhs' rhs'
   in case op of
        Add ->
          case (t1, t2) of
            (TyInt, TyInt) -> pure (TyInt, sexpr)
            (TyInt, TyDouble) -> pure (TyDouble, sexpr)
            (TyDouble, TyInt) -> pure (TyDouble, sexpr)
            (TyDouble, TyDouble) -> pure (TyDouble, sexpr)
            -- FIXME: This error is not true in all cases!
            _ -> throwError $ TypeError loc [TyInt, TyDouble] t1
        Sub ->
          case (t1, t2) of
            (TyInt, TyInt) -> pure (TyInt, sexpr)
            (TyInt, TyDouble) -> pure (TyDouble, sexpr)
            (TyDouble, TyDouble) -> pure (TyDouble, sexpr)
            (TyDouble, TyInt) -> pure (TyDouble, sexpr)
            -- FIXME: This error is not true in all cases!
            _ -> throwError $ TypeError loc [TyInt, TyDouble] t1
        Mul ->
          case (t1, t2) of
            (TyInt, TyInt) -> pure (TyInt, sexpr)
            (TyDouble, TyDouble) -> pure (TyDouble, sexpr)
            (TyInt, TyDouble) -> pure (TyDouble, sexpr)
            (TyDouble, TyInt) -> pure (TyDouble, sexpr)
            _ -> throwError $ TypeError loc [TyInt, TyDouble] t1
        Div ->
          case (t1, t2) of
            (TyInt, TyInt) -> pure (TyInt, sexpr)
            (TyDouble, TyDouble) -> pure (TyDouble, sexpr)
            (TyInt, TyDouble) -> pure (TyDouble, sexpr)
            (TyDouble, TyInt) -> pure (TyDouble, sexpr)
            _ -> throwError $ TypeError loc [TyInt, TyDouble] t1
        -- TODO: These should check extra things, as
        -- currently we can only compare ints and floats
        Eq -> do
          checkTypeSameAndNotVoid loc t1 t2
          pure (TyBoolean, sexpr)
        NEq -> do
          checkTypeSameAndNotVoid loc t1 t2
          pure (TyBoolean, sexpr)
        Greater -> do
          checkTypeSameAndNotVoid loc t1 t2
          pure (TyBoolean, sexpr)
        GreaterEq -> do
          checkTypeSameAndNotVoid loc t1 t2
          pure (TyBoolean, sexpr)
        Less -> do
          checkTypeSameAndNotVoid loc t1 t2
          pure (TyBoolean, sexpr)
        LessEq -> do
          checkTypeSameAndNotVoid loc t1 t2
          pure (TyBoolean, sexpr)
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
  checkedIfBody <- checkInScope (Just "ifBody") [] iBody
  checkedElseBody <- checkInScope (Just "elseBody") [] eBody
  pure (SAIfStatement p checkedIfBody checkedElseBody)
checkStatement (While loc predicate body) = do
  p <- checkExpr predicate
  assertTypeMatch (loc {locColumn = locColumn loc + 6}) TyBoolean (fst p)
  checkedBody <- checkInScope (Just "whileBody") [] body
  pure $ SAWhileStatement p checkedBody

assertTypeMatch :: Location -> Type -> Type -> Semantic ()
assertTypeMatch l t1 t2 =
  unless (t1 == t2 || t1 == TyFlexible) $ throwError $ TypeError l [t1] t2

tryCreateVar :: Location -> Text -> Maybe Type -> Semantic ()
tryCreateVar l name t = do
  env <- get
  let vs = vars env
  -- FIXME: Only error on the latest scope
  let foundVar = findVariable vs name
  when (isJust foundVar) $ throwError $ DuplicateVarDeclaration l name
  put env {vars = addVariable vs (varType, name)}
  where
    varType = fromMaybe TyFlexible t

builtInFunctions :: Functions
builtInFunctions =
  M.fromList $ fmap createFn [("printf", TyVoid, [TyString, TyInt])]
  where
    createFn (name, ret, params) =
      (name, FunctionInterface name ret $ fmap createParam params)
    createParam t = (t, "dummyVar")

checkTypeSameAndNotVoid :: Location -> Type -> Type -> Semantic ()
checkTypeSameAndNotVoid loc t1 t2 = do
  assertTypeMatch loc t1 t2
  when (t1 == TyVoid) $ throwError $ VoidComparisonError loc

checkFunction :: FunctionDef -> Semantic SAFunction
checkFunction (FunctionDef _ name args retTy _ body) = do
  let checkedArgs = fmap makeArg args
  let returnType = fromJust $ typeFromText retTy
  -- Set the current function so we can investigate any
  -- returns in the body
  modify
    (\env ->
       env {currentFunction = FunctionInterface name returnType checkedArgs})
  bodyStatements <- checkInScope (Just $ name <> " function") checkedArgs body
  pure $ SAFunction name checkedArgs returnType bodyStatements
  where
    makeArg (FunctionArg _ n t) = (fromJust $ typeFromText t, n)

checkInScope ::
     Maybe Text -> [(Type, Text)] -> [Statement] -> Semantic [SAStatement]
checkInScope maybeName initVars stmts = do
  scope <- gets vars
  modify (\env -> env {vars = pushScope scope $ Scope name initVars})
  checked <- mapM checkStatement stmts
  modify (\env -> env {vars = popScope (vars env)})
  pure checked
  where
    name = fromMaybe "newScope" maybeName
