module PrologParser where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe
import Parser
import Types

atomP :: Parser TermP
atomP = do
    atom <- expressionP ",.():-#%/*\n"
    return $ FunctionP atom []

functionP :: Parser TermP
functionP = do
    name <- expressionP ",.():-#%/*\n"
    spaceP
    charP '('
    spaceP
    args <- sepByP (charP ',') termP failP
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
    tails <- sepByP (charP ',') termP failP
    spaceP
    charP '.'
    return $ RuleP head tails

commentOneP :: Parser ()
commentOneP = do
    charP '%'
    manyP (/= '\n')
    return ()

commentMultipleP :: Parser ()
commentMultipleP = do
    stringP "/*"
    untilP "*/"
    stringP "*/"
    return ()

programP :: Parser ProgramP
programP = do
    spaceP
    rules <- sepByP spaceP (ruleP <|> factP) (commentOneP <|> commentMultipleP)
    spaceP
    return $ ProgramP rules

queryP :: Parser QueryP
queryP = do
    spaceP
    terms <- sepByP (charP ',') termP failP
    spaceP
    charP '.'
    spaceP
    return $ QueryP terms

processTerm :: TermP -> State VarNames Term
processTerm (FunctionP name [])
    | name == "_" = do
        (VarNames vn) <- get
        put (VarNames (vn ++ ["_"]))
        return $ Var (length vn)
    | isUpper (head name) || head name == '_' = do
        (VarNames vn) <- get
        case elemIndex name vn of
            (Just index) -> return $ Var index
            Nothing -> do
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