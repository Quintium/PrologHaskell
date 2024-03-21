module PrologParser where

import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Parser
import Types

atomP :: Parser TermP
atomP = do
    atom <- expressionP ",.():-\n"
    return $ FunctionP atom []

functionP :: Parser TermP
functionP = do
    name <- expressionP ",.():-\n"
    spaceP
    charP '('
    spaceP
    args <- sepBy (charP ',') termP
    charP ')'
    return $ FunctionP name args

termP :: Parser TermP
termP = do
    spaceP
    term <- functionP <|> atomP
    spaceP
    return term

factP :: Parser RuleP
factP = do
    term <- termP
    charP '.'
    return $ RuleP term []

ruleP :: Parser RuleP
ruleP = do
    head <- termP
    stringP ":-"
    spaceP
    tails <- sepBy (charP ',') termP
    charP '.'
    return $ RuleP head tails

programP :: Parser ProgramP
programP = do
    spaceP
    rules <- sepBy spaceP (ruleP <|> factP)
    spaceP
    return $ ProgramP rules

queryP :: Parser QueryP
queryP = do
    spaceP
    terms <- sepBy (charP ',') termP
    spaceP
    return $ QueryP terms

processTerm :: TermP -> State VarNames Term
processTerm (FunctionP name [])
    | isUpper (head name) || head name == '_' = do
        (VarNames vn) <- get
        if name `elem` vn
            then return $ Var (fromJust (elemIndex name vn))
            else do
                put (VarNames (vn ++ [name]))
                return $ Var (length vn)
    | otherwise = return $ Function name []
processTerm (FunctionP name args) = do
    args' <- mapM processTerm args
    return $ Function name args'

processProgram :: ProgramP -> Program
processProgram (ProgramP rules) = Program $ map processRule rules

processRule :: RuleP -> Rule
processRule (RuleP head tails) =
    evalState (Rule <$> processTerm head <*> mapM processTerm tails) emptyVarNames

processQuery :: QueryP -> Query
processQuery (QueryP ts) =
    let (terms, vn) = runState (mapM processTerm ts) emptyVarNames
     in Query terms vn