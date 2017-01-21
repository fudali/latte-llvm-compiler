module Compiler where

import AbsLatte
{-import ErrM-}
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe
import System.IO

data ExpValue = VInt Int | VReg String

type MyState = (Int, [String])
type Res a = StateT MyState IO a

emptyState :: MyState
emptyState = (0, [])

compile :: Program -> IO String
compile program = do
        (program, _) <- runStateT (compileProgram program) emptyState
        return program

compileProgram :: Program -> Res String
compileProgram (Program topDefs) = compileP topDefs

{-alloca :: String -> Res String-}
{-alloca id = do-}
        {-(nextNumber, list) <- get-}
        {-let reg = regFromId id-}
        {-if (any (==reg) list) then return "" else do-}
                    {-put (nextNumber, reg:list)-}
                    {-return $ reg ++ " = alloca i32\n"-}

compileP :: [TopDef] -> Res String
compileP [] = return ""
compileP (topDef:rest) = do
        currCode <- cFunction topDef
        restCode <- compileP rest
        return $ currCode ++ restCode

cFunction :: TopDef -> Res String
cFunction (FnDef typ (Ident id) args block) = do
        argsCode <- argsHelper args
        blockCode <- cBlock block
        let typCode = typeToCode typ
        return $ "define " ++ typCode ++ " @" ++ id ++ argsCode ++ blockCode 

typeToCode :: Type -> String
typeToCode Int = "i32"

cBlock :: Block -> Res String
cBlock b = return "this is block"

argsHelper :: [Arg] -> Res String
argsHelper [] = return ""
argsHelper ((Arg typ (Ident id):[])) = do
        let argCode = typeToCode typ ++ " %" ++ id
        return argCode

argsHelper ((Arg typ (Ident id)):rest) = do
        let argCode = typeToCode typ ++ " %" ++ id
        restCode <- argsHelper rest
        return $ argCode ++ ", " ++ restCode

{-compileP [] = return "ret i32 0\n"-}
{-compileP ((Ass (Ident id) exp):rest) = do-}
        {-(expCode, val) <- compileExp exp-}
        {-allocaCode <- alloca id-}
        {-restCode <- compileP rest-}
        {-let assPartialCode = case val of-}
                           {-(VInt int) -> show int-}
                           {-(VReg reg) -> reg-}
        {-let assCode = "store i32 " ++ assPartialCode ++ ", i32* " ++ (regFromId id) ++ "\n"-}
        {-return $ allocaCode ++ expCode ++ assCode ++ restCode-}

{-compileP ((Expr exp):rest) = do-}
        {-(expCode, val) <- compileExp exp-}
        {-restCode <- compileP rest-}
        {-let printCode = case val of-}
             {-(VInt int) -> "call void @printInt(i32 " ++ show int ++ ")\n"-}
             {-(VReg reg) -> "call void @printInt(i32 " ++ reg ++ ")\n"-}
        {-return $ expCode ++ printCode ++ restCode-}

{-regFromId :: String -> String-}
{-regFromId id = "%loc_" ++ id-}

{-genReg :: Res String-}
{-genReg = do-}
        {-(nextNumber, list) <- get-}
        {-let newReg = "%t" ++ show nextNumber-}
        {-put (nextNumber + 1, newReg:list)-}
        {-return newReg-}

{-compileExpOp :: Exp -> Exp -> String -> Res (String, ExpValue)-}
{-compileExpOp exp1 exp2 op = do-}
        {-newReg <- genReg-}
        {-(exp1Code, val1) <- compileExp exp1-}
        {-(exp2Code, val2) <- compileExp exp2-}
        {-let partialCode = case (val1, val2) of-}
             {-(VInt int1, VInt int2) -> show int1 ++ ", " ++ show int2-}
             {-(VInt int, VReg reg) -> show int ++ ", " ++ reg-}
             {-(VReg reg, VInt int) -> reg ++ ", " ++ show int-}
             {-(VReg reg1, VReg reg2) -> reg1 ++ ", " ++ reg2-}
        {-let code = exp1Code ++ exp2Code ++ newReg ++ " = " ++ op ++ " i32 " ++ partialCode ++ "\n"-}
        {-return (code, VReg newReg)-}
       
{-compileExp :: Exp -> Res (String, ExpValue)-}
{-compileExp (ExpLit e) = return ("", VInt $ fromIntegral e)-}
{-compileExp (ExpVar (Ident id)) = do-}
        {-reg <- genReg-}
        {-let code = reg ++ " = load i32, i32* " ++ (regFromId id) ++ "\n"-}
        {-return (code, VReg reg)-}
             
{-compileExp (ExpAdd exp1 exp2) = compileExpOp exp1 exp2 "add"-}
{-compileExp (ExpSub exp1 exp2) = compileExpOp exp1 exp2 "sub"-}
{-compileExp (ExpMul exp1 exp2) = compileExpOp exp1 exp2 "mul"-}
{-compileExp (ExpDiv exp1 exp2) = compileExpOp exp1 exp2 "sdiv"-}

