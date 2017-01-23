module Compiler where

import AbsLatte
{-import ErrM-}
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe
import System.IO

data ExpValue = VInt Integer | VBool Bool | VReg String

type LabelNumber = Int
type RegNumber = Int
type MyState = (RegNumber, LabelNumber, [String])
type Res a = StateT MyState IO a
type Code = String
type Hand = String

emptyState :: MyState
emptyState = (0, 0, [])

compile :: Program -> IO String
compile program = do
        (program, _) <- runStateT (compileProgram program) emptyState
        return program

compileProgram :: Program -> Res String
compileProgram (Program topDefs) = compileP topDefs

--
--
regFromId :: String -> String
regFromId id = "%loc_" ++ id

genReg :: Res String
genReg = do
        (nextRegNr, nextLabelNr, list) <- get
        let newReg = "%t" ++ show nextRegNr
        put (nextRegNr + 1, nextLabelNr, newReg:list)
        return newReg

genLabel :: Res String
genLabel = do
        (nextRegNr, nextLabelNr, list) <- get
        let newLabel = "label" ++ show nextLabelNr
        put (nextRegNr, nextLabelNr + 1, list)
        return newLabel

labelToVar :: String -> String
labelToVar label = "%" ++ label

-- BEGINNING OF HELPERS SECTION --
--
--
--
-- -------------------------------
alloca :: String -> Res String
alloca id = do
        (nextNumber, labelN, list) <- get
        let reg = regFromId id
        if (any (==reg) list) then return "" else do
                    put (nextNumber, labelN, reg:list)
                    return $ reg ++ " = alloca i32\n"

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

cStmt (BStmt (Block stmts)) = cStmts stmts

cStmt (Decl typ items) = do
        code <- declareItems typ items
        return code

cStmt (Ass (Ident id) exp) = do
        (expCode, hand) <- cExp exp
        return $ expCode ++ "store i32 " ++ hand ++ ", i32* " ++ (regFromId id) ++ "\n"

cStmt (Incr (Ident id)) = do
        return "incr\n"

cStmt (Decr (Ident id)) = do
        let exp = (EAdd (EVar (Ident id)) Minus (ELitInt 1))
        cStmt (Ass (Ident id) exp)
        {-(loadCode, hand) <- cExp (EVar (Ident id))-}
        {-(decrCode, _) <- cExp (EAdd (EVar (Ident id)) Minus (ELitInt 1))-}
        {-return (loadCode ++ decrCode, hand)-}



{-cExp (EVar (Ident id)) = do-}
{-cExp (EAdd exp1 op exp2) = do-}


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
        let expPart = "\n" ++ expLabel ++ ":\n" ++ expCode
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

-- EXP
--
-- ----------------------

cExp :: Expr -> Res (String, Hand)
cExp (EVar (Ident id)) = do
        newReg <- genReg
        return (newReg ++ " = load i32, i32* " ++ regFromId id ++ "\n", newReg)

cExp ELitTrue = do
        return ("", "1")

cExp ELitFalse = do
        return ("", "0")

cExp (ELitInt int) = do
        return ("", show int)

cExp (EString str) = do
        return ("str\n", show 996)

cExp (Neg exp) = do
        (expCode, hand) <- cExp exp
        reg <- genReg
        return (expCode ++ reg ++ " = sub i32 0, " ++ hand ++ "\n", reg)

cExp (Not exp) = do
        return ("!not\n", "reg")

cExp (EMul exp1 op exp2) = do
        return ("mul*\n", "reg")

cExp (EAdd exp1 op exp2) = do
        (expCode1, hand1) <- cExp exp1
        (expCode2, hand2) <- cExp exp2
        reg <- genReg
        return (expCode1 ++ expCode2 ++ reg ++ " = add i32 " ++ hand1 ++ ", " ++ hand2 ++ "\n", reg)

cExp (ERel exp1 op exp2) = do
        (expCode1, hand1) <- cExp exp1
        (expCode2, hand2) <- cExp exp2
        reg <- genReg
        return (expCode1 ++ expCode2 ++ reg ++ " = icmp " ++ relOpToCode op ++ " i32 " ++ hand1 ++ ", " ++ hand2 ++ "\n", reg)

cExp (EAnd exp1 exp2) = do
        return ("and&&\n", "reg")

cExp (EOr exp1 exp2) = do
        return ("or||\n", "reg")

cExp (EApp (Ident id) exps) = do
        (argsCode, args) <- applyExps exps 
        return (argsCode ++ "call void @" ++ id ++ "(" ++ args ++ ")\n", "0")

