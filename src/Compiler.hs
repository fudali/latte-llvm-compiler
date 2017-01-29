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
type VarEnv = M.Map Var (Type, Loc)
type RegEnv = M.Map String String
type FunEnv = M.Map FunName (Type, [Type])
type StringId = Int
type StringMap = M.Map String StringId

type MyState = ((RegNumber, LabelNumber, Loc, StringId), RegEnv, VarEnv, FunEnv, StringMap, Type)
type Res a = StateT MyState IO a

initialFuns :: FunEnv
initialFuns = M.fromList [("printInt", (Void, [Int])), ("printString", (Void, [Str]))]

initialRegs :: RegEnv
initialRegs = M.empty

initialEnv :: VarEnv
initialEnv = M.empty

initialStringMap :: StringMap
initialStringMap = M.empty

emptyState :: MyState
emptyState = ((0, 0, 0, 0), initialRegs, initialEnv, initialFuns, initialStringMap, Void)

--
--
regFromId :: String -> Res String
regFromId id = do
        (counters, _, env, _, _, _) <- get
        let (typ, loc) = fromMaybe (error ("undefined variable: " ++ id)) $ M.lookup id env
        let name = "%loc_" ++ show loc
        (a, regs, b, c, d, e) <- get
        put (a, M.insert (show loc) (typeToCode typ) regs, b, c, d, e)
        return name

typeFromId :: String -> Res Type
typeFromId id = do
        (_, _, env, _, _, _) <- get
        let (typ, _) = fromMaybe (error ("undefined variable: " ++ id)) $ M.lookup id env
        return typ

getFunType :: FunName -> Res Type
getFunType id = do
        (_, _, _, funs, _, _) <- get
        let (typ, _) = fromMaybe (error $ "undefined function: " ++ id) $ M.lookup id funs
        return typ

getFunArgTypes :: FunName -> Res [Type]
getFunArgTypes id = do
        (_, _, _, funs, _, _) <- get
        let (_, types) = fromMaybe (error $ "undefined function: " ++ id) $ M.lookup id funs
        return types

getTypOfReg :: Hand -> Res String
getTypOfReg hand = do
        (_, regs, _, _, _, _) <- get
        let typ = fromMaybe ("i32") $ M.lookup hand regs
        return typ

getCurrentType :: Res Type
getCurrentType = do
        (_, _, _, _, _, typ) <- get
        return typ

typeToCode :: Type -> String
typeToCode Int = "i32"
typeToCode Str = "i8*"
typeToCode Bool = "i1"
typeToCode Void = "void"

genRegT :: String -> Res String
genRegT typ = do
        reg <- genReg
        (a, regs, b, c, d, e) <- get
        put (a, M.insert reg typ regs, b, c, d, e)
        return reg

genReg :: Res String
genReg = do
        ((nextRegNr, nextLabelNr, nextLoc, nextStrId), regs, env, funs, strs, currTyp) <- get
        let newReg = "%t" ++ show nextRegNr
        put ((nextRegNr + 1, nextLabelNr, nextLoc, nextStrId), regs, env, funs, strs, currTyp)
        return newReg

genLabel :: Res String
genLabel = do
        ((nextRegNr, nextLabelNr, nextLoc, nextStrId), regs, env, funs, strs, currTyp) <- get
        let newLabel = "label" ++ show nextLabelNr
        put ((nextRegNr, nextLabelNr + 1, nextLoc, nextStrId), regs, env, funs, strs, currTyp)
        return newLabel

labelToVar :: String -> String
labelToVar label = "%" ++ label

putStrToState :: String -> Res (String, Int)
putStrToState str = do
        ((nextRegNr, nextLabelNr, nextLoc, nextStrId), regs, env, funs, strs, currTyp) <- get
        let strId = (case (M.lookup str strs) of
                (Just id) -> id
                (Nothing) -> nextStrId
                       )
        put ((nextRegNr, nextLabelNr, nextLoc, nextStrId+1), regs, env, funs, M.insert str strId strs, currTyp)
        return $ ("@.str" ++ show strId, (length str) + 1)


alloca :: String -> Type -> Res String
alloca id typ = do
        ((nextRegNr, nextLabelNr, nextLoc, nextStrId), regs, env, funs, strs, currTyp) <- get
        let reg = "%loc_" ++ show nextLoc
        put ((nextRegNr, nextLabelNr, nextLoc + 1, nextStrId), regs, (M.insert id (typ, nextLoc) env), funs, strs, currTyp)
        return $ reg ++ " = alloca " ++ typeToCode typ ++ "\n"

declareBuiltInMethods :: String
declareBuiltInMethods = 
        printInt ++ printString ++ strcat ++ "\n" where
                printInt = "declare void @printInt(i32);\n"
                printString = "declare void @printString(i8*);\n"
                strcat = "declare i8* @__strcat_chk(i8*, i8*, i32)\n"

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
        allocaCode <- alloca id typ
        reg <- regFromId id
        let store = "store " ++ typeToCode typ ++ " %" ++ id ++ ", " ++ typeToCode typ ++ "* " ++ reg ++ "\n"
        return $ allocaCode ++ store

typFromArg :: Arg -> Type
typFromArg (Arg typ _) = typ

makeStrings :: StringMap -> Code
makeStrings strs = concat $ map makeString $ M.toList strs where
        makeString (string, id) = "@.str" ++ show id ++ " = private unnamed_addr constant [" ++ show ((length string) + 1) ++ " x i8] c\"" ++ string ++ "\\00\"\n"

-- END OF HELPERS SECTION --
--
--
-- -------------------------
compile :: Program -> IO String
compile program = do
        (program, state) <- runStateT (compileProgram program) emptyState
        let (_, _, _, _, stringMap, _) = state
        let strings = makeStrings stringMap
        return $ declareBuiltInMethods ++ program ++ strings

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
registerFun (FnDef typ (Ident id) args _) = do
        (c, r, v, funs, strs, currTyp) <- get
        put (c, r, v, M.insert id (typ, map typFromArg args) funs, strs, currTyp)
        return ()

compileP :: [TopDef] -> Res String
compileP [] = return ""
compileP (topDef:rest) = do
        currCode <- cFunction topDef
        restCode <- compileP rest
        return $ currCode ++ restCode

cFunction :: TopDef -> Res String
cFunction (FnDef typ (Ident id) args (Block stmts)) = do
        (counters, regs, _, funs, strs, _) <- get
        put (counters, regs, M.empty, funs, strs, typ)
        regArgs <- registerArgs args
        argsCode <- declareArgs args
        let argsCodePar = "(" ++ argsCode ++ ")"
        stmtsCode <- cStmts stmts
        let ret = if typ == Str then "" else artificialReturn typ
        let blockCode = "{\n" ++ regArgs ++ stmtsCode ++ ret ++ "}"
        let typCode = typeToCode typ
        return $ "define " ++ typCode ++ " @" ++ id ++ argsCodePar ++ " " ++ blockCode ++ "\n\n"

declareItems :: Type -> [Item] -> Res String
declareItems typ [] = return ""
declareItems typ (item:rest) = do
        itemCode <- declareItem typ item
        restCode <- declareItems typ rest
        return $ itemCode ++ restCode

defaultValue :: Type -> Expr
defaultValue typ = case typ of
                     Int -> ELitInt 0
                     Bool -> ELitFalse
                     Str -> EString ""

declareItem :: Type -> Item -> Res String
declareItem typ (NoInit ident) = declareItem typ (Init ident $ defaultValue typ) 

declareItem typ (Init (Ident id) exp) = do
        allocaCode <- alloca id typ
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
        (_, _, oldEnv, funs, _, _) <- get
        res <- cStmts stmts
        (newCounters, newRegs, _, _, strs, typ) <- get
        put (newCounters, newRegs, oldEnv, funs, strs, typ)
        return res

cStmt (Decl typ items) = do
        code <- declareItems typ items
        return code

cStmt (Ass (Ident id) exp) = do
        (expCode, hand) <- cExp exp
        reg <- regFromId id
        typ <- typeFromId id
        let typeCode = typeToCode typ
        let pointer = typeCode ++ "*"
        return $ expCode ++ "store " ++ typeCode ++ " " ++ hand ++ ", " ++ pointer ++" " ++ reg ++ "\n"

cStmt (Incr (Ident id)) = cCrement id Plus
cStmt (Decr (Ident id)) = cCrement id Minus

cStmt (Ret exp) = do
        (expCode, hand) <- cExp exp
        typ <- getCurrentType
        return $ expCode ++ "ret " ++ typeToCode typ ++ " " ++ hand ++ "\n"

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
        ifReg <- genRegT $ typeToCode Bool
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
        condReg <- genRegT $ typeToCode Bool
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

applyExps :: [Expr] -> [Type] -> Res (Code, String)
applyExps [] _ = return ("", "")
applyExps (exp:[]) (typ:_) = do
        (expCode, arg) <- applyExp exp typ
        return (expCode, arg)
applyExps (exp:restExps) (typ:restTypes) = do
        (expCode, expArg) <- applyExp exp typ
        (restCode, restArgs) <- applyExps restExps restTypes
        return (expCode ++ restCode, expArg ++ ", " ++ restArgs)

applyExp :: Expr -> Type -> Res (Code, String)
applyExp exp typ = do
        (expCode, hand) <- cExp exp
        return (expCode, typeToCode typ ++ " " ++ hand)

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
mulOpToCode Mod = "srem"


cOp :: Expr -> Expr -> Type -> String -> Res (Code, Hand)
cOp exp1 exp2 typ op = do
        (expCode1, hand1) <- cExp exp1
        (expCode2, hand2) <- cExp exp2
        typCode <- getTypOfReg hand1
        if head typCode == '[' || typCode == "i8*" then conStrs (expCode1 ++ expCode2) hand1 hand2 else do
                reg <- genRegT $ typeToCode Int
                return (expCode1 ++ expCode2 ++ reg ++ " = " ++ op ++ " " ++ typeToCode typ ++ " " ++ hand1 ++ ", " ++ hand2 ++ "\n", reg)

conStrs :: Code -> Hand -> Hand -> Res (Code, Hand)
conStrs begCode hand1 hand2 = do
        let len = 80
        let typ = "[" ++ (show len) ++ " x i8]"
        reg <- genRegT typ
        let alloca = reg ++ " = alloca " ++ typ ++ "\n"
        wReg <- genRegT "i8*"
        let beg = wReg ++ " = getelementptr " ++ typ ++ ", " ++ typ ++ "* " ++ reg ++ ", i32 0, i32 0\n"
        fReg <- genRegT "i8*"
        let first = fReg ++ " = call i8* @__strcat_chk(i8* " ++ wReg ++ ", i8* " ++ hand1 ++ ", i32 80)\n"
        sReg <- genRegT "i8*"
        let second = sReg ++ " = call i8* @__strcat_chk(i8* " ++ wReg ++ ", i8* " ++ hand2 ++ ", i32 80)\n"
        return (begCode ++ alloca ++ beg ++ first ++ second, wReg)

-- EXP
--
-- ----------------------

cExp :: Expr -> Res (Code, Hand)

cExp (EApp (Ident id) exps) = do
        argTypes <- getFunArgTypes id
        (argsCode, args) <- applyExps exps argTypes
        typ <- getFunType id
        let typeCode = typeToCode typ
        reg <- genRegT $ typeToCode typ
        let beginning = if typeCode == "void" then "" else reg ++ " = "
        return (argsCode ++ beginning ++ "call " ++ typeCode ++ " @" ++ id ++ "(" ++ args ++ ")\n", reg)

cExp (EVar (Ident id)) = do
        typ <- typeFromId id
        newReg <- genRegT $ typeToCode typ
        locReg <- regFromId id
        let typCode = typeToCode typ
        let pointer = typCode ++ "*"
        return (newReg ++ " = load " ++ typCode ++ ", " ++ pointer ++ " " ++ locReg ++ "\n", newReg)

cExp ELitTrue = do
        return ("", "1")

cExp ELitFalse = do
        return ("", "0")

cExp (ELitInt int) = do
        return ("", show int)

cExp (EString str) = do
        (strConst, len) <- putStrToState str
        let typ = "[" ++ show len ++ " x i8]"
        reg <- genRegT typ
        let code = reg ++ " = getelementptr " ++ typ ++ ", " ++ typ ++ "* " ++ strConst ++ ", i32 0, i32 0\n"
        return (code, reg)

cExp (Neg exp) = do
        (expCode, hand) <- cExp exp
        reg <- genRegT $ typeToCode Int
        return (expCode ++ reg ++ " = sub i32 0, " ++ hand ++ "\n", reg)

cExp (EMul exp1 op exp2) = cOp exp1 exp2 Int $ mulOpToCode op
cExp (EAdd exp1 op exp2) = cOp exp1 exp2 Int $ addOpToCode op
cExp (EAnd exp1 exp2) = cOp exp1 exp2 Bool "and"
cExp (EOr exp1 exp2) = cOp exp1 exp2 Bool "or"

cExp (ERel exp1 op exp2) = do
        (expCode1, hand1) <- cExp exp1
        (expCode2, hand2) <- cExp exp2
        reg <- genRegT $ typeToCode Bool
        typ <- getTypOfReg hand1
        return (expCode1 ++ expCode2 ++ reg ++ " = icmp " ++ relOpToCode op ++ " " ++ typ ++ " " ++ hand1 ++ ", " ++ hand2 ++ "\n", reg)

cExp (Not exp) = do
        (expCode, hand) <- cExp exp
        newReg <- genRegT $ typeToCode Bool
        let code = newReg ++ " = xor i1 1, " ++ hand ++ "\n"
        return (expCode ++ code, newReg)



