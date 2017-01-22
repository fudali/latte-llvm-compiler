module Compiler where

import AbsLatte
{-import ErrM-}
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe
import System.IO

data ExpValue = VInt Integer | VReg String

type MyState = (Int, [String])
type Res a = StateT MyState IO a
type Code = String

emptyState :: MyState
emptyState = (0, [])

compile :: Program -> IO String
compile program = do
        (program, _) <- runStateT (compileProgram program) emptyState
        return program

compileProgram :: Program -> Res String
compileProgram (Program topDefs) = compileP topDefs

-- BEGINNING OF HELPERS SECTION --
--
--
--
-- -------------------------------
alloca :: String -> Res String
alloca id = do
        (nextNumber, list) <- get
        let reg = regFromId id
        if (any (==reg) list) then return "" else do
                    put (nextNumber, reg:list)
                    return $ reg ++ " = alloca i32\n"

regFromId :: String -> String
regFromId id = "%loc_" ++ id

genReg :: Res String
genReg = do
        (nextNumber, list) <- get
        let newReg = "%t" ++ show nextNumber
        put (nextNumber + 1, newReg:list)
        return newReg

declareArgs :: [Arg] -> Res String
declareArgs [] = return ""
declareArgs ((Arg typ (Ident id):[])) = do
        let argCode = typeToCode typ ++ " %" ++ id
        return argCode

declareArgs ((Arg typ (Ident id)):rest) = do
        let argCode = typeToCode typ ++ " %" ++ id
        restCode <- declareArgs rest
        return $ argCode ++ ", " ++ restCode

typeToCode :: Type -> String
typeToCode Int = "i32"
typeToCode Str = "i8*"
typeToCode Bool = "i1"
typetoCode Void = "void"

declareBuiltInMethods :: String
declareBuiltInMethods = 
        printInt ++ printStr ++ readInt ++ readStr ++ "\n" where
                printInt = "declare void @printInt(i32);\n"
                printStr = "declare void @printStr(i32);\n"
                readInt = "declare i32 @readInt();\n"
                readStr = "declare i32 @readStr();\n"

-- END OF HELPERS SECTION --
--
--
--
-- -------------------------

compileP :: [TopDef] -> Res String
compileP [] = return ""
compileP (topDef:rest) = do
        currCode <- cFunction topDef
        restCode <- compileP rest
        return $ declareBuiltInMethods ++ currCode ++ restCode

cFunction :: TopDef -> Res String
cFunction (FnDef typ (Ident id) args block) = do
        argsCode <- declareArgs args
        let argsCodePar = "(" ++ argsCode ++ ")"
        blockCode <- cBlock block
        let typCode = typeToCode typ
        return $ "define " ++ typCode ++ " @" ++ id ++ argsCodePar ++ " " ++ blockCode 

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

cBlock :: Block -> Res String
cBlock (Block stmts) = do
        stmtsCode <- cStmts stmts
        return $ "{\n" ++ stmtsCode ++ "}"

cStmts :: [Stmt] -> Res String
cStmts [] = return ""
cStmts (st:rest) = do
        stCode <- cStmt st
        restCode <- cStmts rest
        return $ stCode ++ restCode

cStmt :: Stmt -> Res String
cStmt Empty = do
        return ""

cStmt (BStmt b) = do
        cBlock b

cStmt (Decl typ items) = do
        code <- declareItems typ items
        return code

cStmt (Ass (Ident id) exp) = do
        (expCode, val) <- cExp exp
        let assPartialCode = case val of
                           (VInt int) -> show int
                           (VReg reg) -> reg
        return $ "store i32 " ++ assPartialCode ++ ", i32* " ++ (regFromId id) ++ "\n"
        

cStmt (Incr (Ident id)) = do
        return "incr\n"

cStmt (Decr (Ident id)) = do
        return "decr\n"

cStmt (Ret exp) = do
        (expCode, val) <- cExp exp
        return $ "ret i32 0" ++ expCode

cStmt VRet = do
        return "ret void\n"

cStmt (Cond exp stmt) = do
        return "cond\n"

cStmt (CondElse exp stTrue stFalse) = do
        return "\n"

cStmt (While exp st) = do
        return "while\n"

cStmt (SExp exp) = do
        (expCode, val) <- cExp exp
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
        (expCode, val) <- cExp exp
        let reference = case val of
                      (VInt int) -> show int
                      (VReg reg) -> reg
        return (expCode, "i32 " ++ reference)

-- EXP
--
-- ----------------------

cExp :: Expr -> Res (String, ExpValue)
cExp (EVar (Ident id)) = do
        newReg <- genReg
        return (newReg ++ " = load i32, i32* " ++ regFromId id ++ "\n", VReg newReg)

cExp (ELitInt int) = do
        return ("\n", VInt int)

cExp ELitTrue = do
        return ("true\n", VInt 1)

cExp ELitFalse = do
        return ("false\n", VInt 0)

cExp (EApp (Ident id) exps) = do
        (argsCode, args) <- applyExps exps 
        return (argsCode ++ "call void @" ++ id ++ "(" ++ args ++ ")\n", VInt 0)

cExp (EString str) = do
        return ("str\n", VInt 996)

cExp (Neg exp) = do
        return ("-neg\n", VReg "reg")

cExp (Not exp) = do
        return ("!not\n", VReg "reg")

cExp (EMul exp1 op exp2) = do
        return ("mul*\n", VReg "reg")

cExp (EAdd exp1 op exp2) = do
        return ("add+\n", VReg "reg")

cExp (ERel exp1 op exp2) = do
        return ("rel<>\n", VReg "reg")

cExp (EAnd exp1 exp2) = do
        return ("and&&\n", VReg "reg")

cExp (EOr exp1 exp2) = do
        return ("or||\n", VReg "reg")


