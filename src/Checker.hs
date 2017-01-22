module Checker where

import AbsLatte
import qualified Data.Map as M
import Control.Monad.State
{-import Data.Maybe-}
{-import System.IO-}
import Control.Monad.Trans.Except

type Var = String
type MyState = (VarEnv, FunEnv, Type)

type VarEnv = M.Map Var Type
type FunEnv = M.Map Var ([Arg], Type) 

type Res = ExceptT String (State MyState) Type
type ResRet = ExceptT String (State MyState) (Type, Bool)

emptyVarEnv :: VarEnv
emptyVarEnv = M.empty

printInt = ("printInt", ([Arg Int (Ident "int")], Void))
printStr = ("printString", ([Arg Str (Ident "str")], Void))
readInt = ("readInt", ([], Int))
readStr = ("readString", ([], Str))
emptyFunEnv :: FunEnv
emptyFunEnv = M.fromList [printInt, printStr, readInt, readStr]

emptyState :: MyState
emptyState = (emptyVarEnv, emptyFunEnv, Void)

typeEC :: Type -> Type -> String
typeEC expected given = "Type error -- EXPECTED: " ++ show expected ++ ", GIVEN: " ++ show given

checkProgram :: Program -> IO ()
checkProgram p = case runChecker p of
                   (Right e, _) -> return ()
                   (Left e, _) -> do
                           error e

runChecker program = runR . runExceptT $ checkP program
runR r = runState r emptyState

checkP :: Program -> Res
checkP (Program topDefs) = do
        registerFunctions topDefs
        checkFunctions topDefs

registerFunctions :: [TopDef] -> Res
registerFunctions [] = return Void
registerFunctions ((FnDef typ (Ident id) args _):rest) = catchE (do
        registerFunctions rest
        (env, funs, retTyp) <- get
        checkArgs args
        let newf = (args, typ)
        put (env, M.insert id newf funs, retTyp)
        return Void
        ) handler where
                handler e = throwE $ e ++ " in\n\tfunction declaration " ++ id

checkFunctions :: [TopDef] -> Res
checkFunctions [] = return Void
checkFunctions (topDef:rest) = do
        checkFunction topDef
        checkFunctions rest

checkFunction :: TopDef -> Res
checkFunction (FnDef typ (Ident id) args (Block stmts)) = catchE (do
        (_, funs, _) <- get
        put (emptyVarEnv, funs, typ)
        putArgs args
        (_, isRet) <- checkStmts stmts
        {-throwE $ "typ funkcji: " ++ show typ ++ " typ returna: " ++ show retTyp ++ " byl return?: " ++ show isRet-}
        {-throwE $ "oczekuje returna o typie " ++ show typ-}
        assertReturn typ isRet
        return typ
        ) handler where
                handler e = throwE $ e ++ " in\n\tfunction " ++ id


checkStmts :: [Stmt] -> ResRet
checkStmts [] = return (Void, False)
checkStmts (st:rest) = catchE (do
        {-(_, _, retTyp) <- get-}
        {-throwE $ "chce returna o typie: " ++ show retTyp ++ " i zaraz wejde do st: " ++ show st-}
        (_, isRetSingle) <- checkS st
        (_, isRetRest) <- checkStmts rest
        return (Void, isRetSingle || isRetRest)
        ) handler where
                handler e = throwE $ e ++ " in STATEMENT\n\t" ++ show st

checkS :: Stmt -> ResRet
checkS Empty = return (Void, False)

checkS (BStmt (Block stmts)) = do
        state <- get
        result <- checkStmts stmts
        put state
        return result

checkS (Decl typ items) = do
        declareItems typ items
        return (typ, False)

checkS (Ass (Ident id) exp) = do
        (env, _, _) <- get
        expT <- evalHigh exp
        case M.lookup id env of
          (Just t) -> do
                  assert t expT
                  return (Void, False)
          Nothing -> throwE $ "undefined variable: " ++ id

checkS (Incr (Ident id)) = do
        (env, _, _) <- get
        case M.lookup id env of
          (Just t) -> do
                  assert Int t
                  return (Void, False)
          Nothing -> throwE $ "undefined variable: " ++ id

checkS (Decr (Ident id)) = do
        (env, _, _) <- get
        case M.lookup id env of
          (Just t) -> do
                  assert Int t
                  return (Void, False)
          Nothing -> throwE $ "undefined variable: " ++ id

checkS (Ret exp) = do
        t <- evalHigh exp
        (_, _, retTyp) <- get
        assert retTyp t
        return (retTyp, True)

checkS VRet = do
        (_, _, retTyp) <- get
        assert retTyp Void
        return (Void, True)

checkS (Cond exp st) = do
        expT <- evalHigh exp
        assert Bool expT
        (_, isRet) <- checkS st
        if exp == ELitTrue then
                return (Void, isRet)
        else
                return (Void, False)

checkS (CondElse exp stTrue stFalse) = do
        expT <- evalHigh exp
        assert Bool expT
        (_, isRet1) <- checkS stTrue
        (_, isRet2) <- checkS stFalse
        if exp == ELitTrue then
                return (Void, isRet1)
        else if exp == ELitFalse then
                return (Void, isRet2)
        else
                return (Void, isRet1 && isRet2)

checkS (While exp st) = do
        expT <- evalHigh exp
        assert Bool expT
        checkS st

checkS (SExp exp) = do
        t <- evalHigh exp
        return (t, False)

-- EXPRESSIONS

evalHigh :: Expr -> Res
evalHigh exp =
        catchE (evalExp exp) handler where
                handler e = throwE $ e ++ " in EXPRESSION\n\t" ++ show exp

evalExp :: Expr -> Res
evalExp (EVar (Ident id)) = do
        (env, _, _) <- get
        case M.lookup id env of
          (Just t) -> return t
          {-(Just t) -> throwE $ "znalazlem w env zmienna " ++ id ++ " o typie " ++ show t-}
          Nothing -> throwE $ "undefined variable: " ++ id

evalExp (ELitInt int) = return Int

evalExp (ELitTrue) = return Bool

evalExp (ELitFalse) = return Bool

evalExp (EApp (Ident id) exps) = do
        (env, funs, _) <- get
        case M.lookup id funs of
          (Just (args, t)) -> do
                  applyArgs args exps
                  return t
          Nothing -> throwE $ "undefined function: " ++ id

evalExp (EString str) = return Str

evalExp (Neg exp) = do
        t <- evalExp exp
        assert Int t

evalExp (Not exp) = do
        t <- evalExp exp
        assert Bool t

evalExp (EMul exp1 _ exp2) = do
        t1 <- evalExp exp1
        t2 <- evalExp exp2
        assert Int t1
        assert Int t2

evalExp (EAdd exp1 _ exp2) = do
        t1 <- evalExp exp1
        t2 <- evalExp exp2
        assert t1 t2

evalExp (ERel exp1 _ exp2) = do
        t1 <- evalExp exp1
        t2 <- evalExp exp2
        assert t1 t2
        return Bool

evalExp (EAnd exp1 exp2) = do
        t1 <- evalExp exp1
        t2 <- evalExp exp2
        assert Bool t1
        assert Bool t2

evalExp (EOr exp1 exp2) = do
        t1 <- evalExp exp1
        t2 <- evalExp exp2
        assert Bool t1
        assert Bool t2

-- HELPERS
declareItems :: Type -> [Item] -> Res
declareItems _ [] = return Void
declareItems typ ((NoInit (Ident id)):rest) = do
        declareItem typ id
        declareItems typ rest

declareItems typ ((Init (Ident id) exp):rest) = do
        expTyp <- evalHigh exp
        assert typ expTyp
        declareItem typ id
        declareItems typ rest

declareItem :: Type -> String -> Res
declareItem typ id = do
        (env, funs, retTyp) <- get
        put (M.insert id typ env, funs, retTyp)
        return Void

typeErr :: Type -> Type -> String
typeErr expected given = "Type error -- EXPECTED: " ++ show expected ++ ", GIVEN: " ++ show given

assert :: Type -> Type -> Res
assert expected given = do
  if (expected == given) then
    return expected
  else
    throwE $ typeErr expected given

assertReturn :: Type -> Bool -> Res
assertReturn Void _ = return Void
assertReturn _ isRet = do
        if isRet then
                return Void
        else
                throwE $ "return statement is missing"


applyArgs :: [Arg] -> [Expr] -> Res
applyArgs [] [] = return Void
applyArgs (_:_) [] = throwE $ "too few arguments passed"
applyArgs [] (_:_) = throwE $ "too many arguments passed"
applyArgs (arg@(Arg typ (Ident id)):args) (exp:exps) = catchE (do
        t <- evalHigh exp
        assert typ t
        applyArgs args exps
        return t
       ) handler where
               handler e = throwE $ e ++ " in\n\t" ++ "PASSING arguments " ++ show arg

putArgs :: [Arg] -> Res
putArgs [] = return Void
putArgs ((Arg typ (Ident id)):rest) = do
        (env, funs, retTyp) <- get
        put (M.insert id typ env, funs, retTyp)
        putArgs rest
        return typ
        
checkArgs :: [Arg] -> Res
checkArgs args = checkArgsHelper args []

checkArgsHelper :: [Arg] -> [String] -> Res
checkArgsHelper [] _ = return Void
checkArgsHelper ((Arg _ (Ident id)):rest) list = do
        if (elem id list) then
                throwE $ id ++ " is declared more than once"
        else
                checkArgsHelper rest (id:list)


