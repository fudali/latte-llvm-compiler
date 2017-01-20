-- automatically generated by BNF Converter
module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import Data.List.Split
import Data.List


import LexLatte
import ParLatte
import SkelLatte
import PrintLatte
import AbsLatte

import Checker
import Compiler


import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v f p

makeNewName :: String -> String
makeNewName name = do
        let parts = splitOn "/" name
        let fileName = replace ".ins" ".ll" $ last parts
        intercalate "/" $ (init parts) ++ [fileName]

replace :: String -> String -> String -> String
replace old new t = intercalate new $ splitOn old t

run :: Verbosity -> String -> ParseFun Program -> String -> IO ()
run v ff p s = let ts = myLexer s in case p ts of
        Bad s -> do
                putStrLn "\nParse Failed...\n"
                putStrLn s
                exitFailure
        Ok tree -> do
                checkProgram tree                
                {-template <- readFile "./lib/template.ll"-}
                {-main <- compile tree-}
                {-let program = replace "#PROGRAM#" main template-}
                {-writeFile (makeNewName ff) program-}
                exitSuccess


main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> do
                        putStrLn "\nCompiling Failed... No argument provided...\n"
                        exitFailure
                fs -> mapM_ (runFile 2 pProgram) fs

