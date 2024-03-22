module Main where

import Control.Monad

import Types
import Parser
import PrologParser
import Solve

parseFile :: String -> IO (Maybe Program)
parseFile path = do
    text <- readFile path
    let res = do
            program <- finishParser programP text
            return $ processProgram program
    return res

consultFile :: String -> String -> IO (Maybe [String])
consultFile path q = do
    file <- parseFile path
    return $ file >>= (`solveQuery` q)

queryLoop :: Program -> IO ()
queryLoop p = do
    putStr "?- "
    q <- getLine
    let resMaybe = solveQuery p q
    (case resMaybe of
        (Just res) -> do
            answerLoop res
            putStrLn ""
        Nothing -> return ())
    queryLoop p

answerLoop :: [String] -> IO ()
answerLoop [] = do
    putStrLn "false."
answerLoop (r:res) = do
    putStr r
    putStr " "
    c <- getLine
    when (head c == ';') $ answerLoop res

main :: IO ()
main = do
    putStr "File name: "
    fileName <- getLine
    programMaybe <- parseFile fileName
    case programMaybe of
        (Just program) -> queryLoop program
        Nothing -> putStrLn "Parse error"