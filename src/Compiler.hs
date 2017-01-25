module Compiler where

import AbsLatte
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe
import System.IO
import Control.Monad.Trans.Except

data ExpValue = VInt Integer | VBool Bool | VReg String

type LabelNumber = Int
type RegNumber = Int
type Code = String
type Hand = String
type Var = String
type FunName = String
type Loc = Int
type VarEnv = M.Map Var Loc
type FunEnv = M.Map FunName Type

type MyState = ((RegNumber, LabelNumber, Loc), VarEnv, FunEnv)
type Res a = StateT MyState IO a

initialFuns :: FunEnv
initialFuns = M.fromList [("printInt", Void), ("printString", Void)]

initialEnv :: VarEnv
initialEnv = M.empty

emptyState :: MyState
emptyState = ((0, 0, 0), initialEnv, initialFuns)

--
--
regFromId :: String -> Res String
regFromId id = do
        (counters, env, _) <- get
        let loc = fromMaybe (error ("undefined variable: " ++ id)) $ M.lookup id env
        return $ "%loc_" ++ show loc

getFunTypeCode :: FunName -> Res String
getFunTypeCode id = do
        (_, _, funs) <- get
        let typ = fromMaybe (error $ "undefined function: " ++ id) $ M.lookup id funs
        return $ typeToCode typ

typeToCode :: Type -> String
typeToCode Int = "i32"
typeToCode Str = "i8*"
typeToCode Bool = "i1"
typeToCode Void = "void"

genReg :: Res String
genReg = do
        ((nextRegNr, nextLabelNr, nextLoc), env, funs) <- get
        let newReg = "%t" ++ show nextRegNr
        put ((nextRegNr + 1, nextLabelNr, nextLoc), env, funs)
        return newReg

genLabel :: Res String
genLabel = do
        ((nextRegNr, nextLabelNr, nextLoc), env, funs) <- get
        let newLabel = "label" ++ show nextLabelNr
        put ((nextRegNr, nextLabelNr + 1, nextLoc), env, funs)
        return newLabel

labelToVar :: String -> String
labelToVar label = "%" ++ label

alloca :: String -> Res String
alloca id = do
        ((nextRegNr, nextLabelNr, nextLoc), env, funs) <- get
        let reg = "%loc_" ++ show nextLoc
        put ((nextRegNr, nextLabelNr, nextLoc + 1), (M.insert id nextLoc env), funs)
        return $ reg ++ " = alloca i32\n"

declareBuiltInMethods :: String
declareBuiltInMethods = 
        printInt ++ printStr ++ readInt ++ readStr ++ "\n" where
                printInt = "declare void @printInt(i32);\n"
                printStr = "declare void @printStr(i32);\n"
                readInt = "declare i32 @readInt();\n"
                readStr = "declare i32 @readStr();\n"

artificialReturn :: Type -> Code
artificialReturn typ = "ret " ++ typeToCode typ ++ (if typ /= Void then " 0" else "") ++ "\n"

declareArgs :: [Arg] -> Res String
declareArgs [] = return ""
declareArgs ((Arg typ (Ident id):[])) = do
        let argCode = typeToCode typ ++ " %" ++ id
        return argCode

declareArgs ((Arg typ (Ident id)):rest) = do
        let argCode = typeToCode typ ++ " %" ++ id
        restCode <- declareArgs rest
        return $ argCode ++ ", " ++ restCode
        
registerArgs :: [Arg] -> Res Code
registerArgs [] = return ""
registerArgs (arg:rest) = do
        code <- registerArg arg
        restCode <- registerArgs rest
        return $ code ++ restCode

registerArg (Arg typ (Ident id)) = do
        allocaCode <- alloca id
        reg <- regFromId id
        let store = "store i32 %" ++ id ++ ", i32* " ++ reg ++ "\n"
        return $ allocaCode ++ store

-- END OF HELPERS SECTION --
--
--
-- -------------------------
compile :: Program -> IO String
compile program = do
        (program, _) <- runStateT (compileProgram program) emptyState
        return $ declareBuiltInMethods ++ program

compileProgram :: Program -> Res Code
compileProgram (Program topDefs) = do
        registerFuns topDefs
        compileP topDefs

registerFuns :: [TopDef] -> Res ()
registerFuns [] = return ()
registerFuns (fun:rest) = do
        registerFun fun
        registerFuns rest

registerFun :: TopDef -> Res ()
registerFun (FnDef typ (Ident id) _ _) = do
        (c, v, funs) <- get
        put (c, v, M.insert id typ funs)
        return ()

compileP :: [TopDef] -> Res String
compileP [] = return ""
compileP (topDef:rest) = do
        (counters, _, funs) <- get
        put (counters, M.empty, funs)
        currCode <- cFunction topDef
        restCode <- compileP rest
        return $ currCode ++ restCode

cFunction :: TopDef -> Res String
cFunction (FnDef typ (Ident id) args (Block stmts)) = do
        regArgs <- registerArgs args
        argsCode <- declareArgs args
        let argsCodePar = "(" ++ argsCode ++ ")"
        stmtsCode <- cStmts stmts
        let blockCode = "{\n" ++ regArgs ++ stmtsCode ++ artificialReturn typ ++ "}"
        let typCode = typeToCode typ
        return $ "define " ++ typCode ++ " @" ++ id ++ argsCodePar ++ " " ++ blockCode ++ "\n\n"

declareItems :: Type -> [Item] -> Res String
declareItems typ [] = return ""
declareItems typ (item:rest) = do
        itemCode <- declareItem typ item
        restCode <- declareItems typ rest
        return $ itemCode ++ restCode

declareItem :: Type -> Item -> Res String
declareItem typ (NoInit ident) = declareItem typ (Init ident (ELitInt 0)) 

declareItem typ (Init (Ident id) exp) = do
        allocaCode <- alloca id
        assCode <- cStmt $ Ass (Ident id) exp
        return $ allocaCode ++ assCode

cCrement :: String -> AddOp -> Res Code
cCrement id operator = do
        let exp = (EAdd (EVar (Ident id)) operator (ELitInt 1))
        cStmt (Ass (Ident id) exp)
        {-in case we want to have it as an expr-}
        {-(loadCode, hand) <- cExp (EVar (Ident id))-}
        {-(decrCode, _) <- cExp (EAdd (EVar (Ident id)) Minus (ELitInt 1))-}
        {-return (loadCode ++ decrCode, hand)-}

cStmts :: [Stmt] -> Res String
cStmts [] = return ""
cStmts (st:rest) = do
        stCode <- cStmt st
        restCode <- cStmts rest
        return $ stCode ++ restCode

cStmt :: Stmt -> Res String
cStmt Empty = do
        return ""

cStmt (BStmt (Block stmts)) = do
        (_, oldEnv, funs) <- get
        res <- cStmts stmts
        (newCounters, _, _) <- get
        put (newCounters, oldEnv, funs)
        return res

cStmt (Decl typ items) = do
        code <- declareItems typ items
        return code

cStmt (Ass (Ident id) exp) = do
        (expCode, hand) <- cExp exp
        reg <- regFromId id
        return $ expCode ++ "store i32 " ++ hand ++ ", i32* " ++ reg ++ "\n"

cStmt (Incr (Ident id)) = cCrement id Plus
cStmt (Decr (Ident id)) = cCrement id Minus

cStmt (Ret exp) = do
        (expCode, hand) <- cExp exp
        return $ expCode ++ "ret i32 " ++ hand ++ "\n"

cStmt VRet = do
        return "ret void\n"

cStmt (Cond exp stmt) = cStmt(CondElse exp stmt (Empty))

cStmt (CondElse exp stTrue stFalse) = do
        (expCode, hand) <- cExp exp
        trueBranch <- cStmt stTrue
        falseBranch <- cStmt stFalse
        trueLabel <- genLabel
        falseLabel <- genLabel
        contLabel <- genLabel
        ifReg <- genReg
        let gotoCont = "br label %" ++ contLabel ++ "\n"
        let cmpCode = ifReg ++ " = icmp eq i1 " ++ hand ++ ", 1\n"
        let ifCode = "br i1 " ++ ifReg ++ ", label %" ++ trueLabel ++ ", label %" ++ falseLabel ++ "\n"
        let truePart = "\n" ++ trueLabel ++ ":\n" ++ trueBranch ++ gotoCont
        let falsePart = "\n" ++ falseLabel ++ ":\n" ++ falseBranch ++ gotoCont
        let contPart = "\n" ++ contLabel ++ ":\n"
        return $ expCode ++ cmpCode ++ ifCode ++ truePart ++ falsePart ++ contPart


cStmt (While exp st) = do
        (expCode, hand) <- cExp exp
        bodyCode <- cStmt st
        expLabel <- genLabel
        bodyLabel <- genLabel
        contLabel <- genLabel
        condReg <- genReg
        let gotoExp = "br label %" ++ expLabel ++ "\n"
        let bodyPart = "\n" ++ bodyLabel ++ ":\n" ++ bodyCode ++ gotoExp
        let expPart = "\nbr label %" ++ expLabel ++ "\n" ++ expLabel ++ ":\n" ++ expCode
        let cmpCode = condReg ++ " = icmp eq i1 " ++ hand ++ ", 1\n"
        let brCode = "br i1 " ++ condReg ++ ", label %" ++ bodyLabel ++ ", label %" ++ contLabel ++ "\n"
        let contPart = "\n" ++ contLabel ++ ":\n"
        return $ expPart ++ cmpCode ++ brCode ++ bodyPart ++ contPart

cStmt (SExp exp) = do
        (expCode, _) <- cExp exp
        return expCode

-- EXPS HELPERS
--
-- ----------------------

applyExps :: [Expr] -> Res (Code, String)
applyExps [] = return ("", "")
applyExps (exp:[]) = do
        (expCode, arg) <- applyExp exp
        return (expCode, arg)
applyExps (exp:rest) = do
        (expCode, expArg) <- applyExp exp
        (restCode, restArgs) <- applyExps rest
        return (expCode ++ restCode, expArg ++ ", " ++ restArgs)

applyExp :: Expr -> Res (Code, String)
applyExp exp = do
        (expCode, hand) <- cExp exp
        return (expCode, "i32 " ++ hand)

relOpToCode :: RelOp -> String
relOpToCode LTH = "slt"
relOpToCode LE = "sle"
relOpToCode GTH = "sgt"
relOpToCode GE = "sge"
relOpToCode EQU = "eq"
relOpToCode NE = "ne"

addOpToCode :: AddOp -> String
addOpToCode Plus = "add"
addOpToCode Minus = "sub"

mulOpToCode :: MulOp -> String
mulOpToCode Times = "mul"
mulOpToCode Div = "sdiv"
mulOpToCode Mod = "mod"

cOp :: Expr -> Expr -> String -> Res (Code, Hand)
cOp exp1 exp2 op = do
        (expCode1, hand1) <- cExp exp1
        (expCode2, hand2) <- cExp exp2
        reg <- genReg
        return (expCode1 ++ expCode2 ++ reg ++ " = " ++ op ++ " i32 " ++ hand1 ++ ", " ++ hand2 ++ "\n", reg)


-- EXP
--
-- ----------------------

cExp :: Expr -> Res (Code, Hand)

cExp (EApp (Ident id) exps) = do
        (argsCode, args) <- applyExps exps 
        typeCode <- getFunTypeCode id
        return (argsCode ++ "call " ++ typeCode ++ " @" ++ id ++ "(" ++ args ++ ")\n", "0")

cExp (EVar (Ident id)) = do
        newReg <- genReg
        locReg <- regFromId id
        return (newReg ++ " = load i32, i32* " ++ locReg ++ "\n", newReg)

cExp ELitTrue = do
        return ("", "1")

cExp ELitFalse = do
        return ("", "0")

cExp (ELitInt int) = do
        return ("", show int)

cExp (EString str) = do
        return ("", show 996)

cExp (Neg exp) = do
        (expCode, hand) <- cExp exp
        reg <- genReg
        return (expCode ++ reg ++ " = sub i32 0, " ++ hand ++ "\n", reg)

cExp (EMul exp1 op exp2) = cOp exp1 exp2 $ mulOpToCode op
cExp (EAdd exp1 op exp2) = cOp exp1 exp2 $ addOpToCode op
cExp (EAnd exp1 exp2) = cOp exp1 exp2 "and"
cExp (EOr exp1 exp2) = cOp exp1 exp2 "or"

cExp (ERel exp1 op exp2) = do
        (expCode1, hand1) <- cExp exp1
        (expCode2, hand2) <- cExp exp2
        reg <- genReg
        return (expCode1 ++ expCode2 ++ reg ++ " = icmp " ++ relOpToCode op ++ " i32 " ++ hand1 ++ ", " ++ hand2 ++ "\n", reg)

cExp (Not exp) = do
        (expCode, hand) <- cExp exp
        newReg <- genReg
        let code = newReg ++ " = xor i1 1, " ++ hand ++ "\n"
        return (expCode ++ code, newReg)



