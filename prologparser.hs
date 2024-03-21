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
atomP = (`FunctionP` []) <$> expressionP ",.():-\n"

functionP :: Parser TermP
functionP =
    FunctionP
        <$> (expressionP ",.():-\n" <* spaceP <* charP '(' <* spaceP)
        <*> sepBy (charP ',') termP
        <* charP ')'

termP :: Parser TermP
termP = spaceP *> (functionP <|> atomP) <* spaceP

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

parseTerm :: String -> Maybe (State VarNames Term)
parseTerm s = do
    tp <- finishParser termP s
    return $ processTerm tp

factP :: Parser RuleP
factP = (`RuleP` []) <$> termP <* charP '.'

ruleP :: Parser RuleP
ruleP =
    RuleP
        <$> (termP <* stringP ":-" <* spaceP)
        <*> sepBy (charP ',') termP
        <* charP '.'

programP :: Parser ProgramP
programP = spaceP *> (ProgramP <$> sepBy spaceP (ruleP <|> factP)) <* spaceP

queryP :: Parser QueryP
queryP = spaceP *> (QueryP <$> sepBy (charP ',') termP) <* spaceP

processProgram :: ProgramP -> Program
processProgram (ProgramP rules) = Program $ map processRule rules

processRule :: RuleP -> Rule
processRule (RuleP head tails) =
    evalState (Rule <$> processTerm head <*> mapM processTerm tails) emptyVarNames

processQuery :: QueryP -> Query
processQuery (QueryP ts) =
    let (terms, vn) = runState (mapM processTerm ts) emptyVarNames
     in Query terms vn