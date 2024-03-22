module Main where

import Control.Monad
import Data.Maybe

import Parser
import PrologParser
import Solve
import Types

parseFile :: String -> IO (Maybe Program)
parseFile path = do
    text <- readFile path
    let res = do
            program <- finishParser programP text
            return $ processProgram program
    return res

isHalt :: String -> Bool
isHalt s = isJust $ finishParser (spaceP *> stringP "halt" *> spaceP *> charP '.' *> spaceP) s

queryLoop :: Program -> IO ()
queryLoop p = do
    putStr "?- "
    q <- getLine
    if isHalt q
        then return ()
        else do
            let resMaybe = solveQuery p q
            case resMaybe of
                (Just res) -> do
                    answerLoop res
                    putStrLn ""
                Nothing -> do
                    putStrLn "ERROR: Couldn't parse query"
            queryLoop p

answerLoop :: [String] -> IO ()
answerLoop [] = do
    putStrLn "false."
answerLoop (r : res) = do
    putStr r
    putStr " "
    c <- getLine
    when (not (null c) && head c == ';') $ answerLoop res

main :: IO ()
main = do
    putStr "File name: "
    fileName <- getLine
    programMaybe <- parseFile fileName
    case programMaybe of
        (Just program) -> queryLoop program
        Nothing -> putStrLn $ "ERROR: Couldn't parse " ++ fileName