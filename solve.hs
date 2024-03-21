module Unify where

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Parser
import PrologParser
import Types

unify :: Term -> Term -> Either UnifyFailure Subst
unify (Var v) t = varSubst v t
unify t (Var v) = varSubst v t
unify (Function s1 ts1) (Function s2 ts2) = fctSubst s1 ts1 s2 ts2

varSubst :: Int -> Term -> Either UnifyFailure Subst
varSubst v t
    | varEq v t = Right emptySubst
    | occurs v t = Left OccurFailure
    | otherwise = Right (Subst [v] s)
  where
    s (Var n) = if n == v then t else Var n
    s (Function str ts) = Function str (map s ts)

occurs :: Int -> Term -> Bool
occurs v1 (Var v2) = v1 == v2
occurs v (Function s ts) = any (occurs v) ts

fctSubst :: String -> [Term] -> String -> [Term] -> Either UnifyFailure Subst
fctSubst s1 ts1 s2 ts2
    | s1 /= s2 = Left ClashFailure
    | length ts1 /= length ts2 = Left ClashFailure
    | otherwise = foldl substStep (Right emptySubst) (zip ts1 ts2)

substStep :: Either UnifyFailure Subst -> (Term, Term) -> Either UnifyFailure Subst
substStep unifyResult (t1, t2) = do
    s1 <- unifyResult
    s2 <- unify (applySubst s1 t1) (applySubst s1 t2)
    return $ chainSubst s1 s2

solve :: Program -> [Term] -> State Int [Subst]
solve _ [] = return [emptySubst]
solve (Program rules) (q : qs) = do
    maxVar <- get
    return $ concatMap (\rule -> evalState (solveRule rule) maxVar) rules
  where
    solveRule rule = do
        substMaybe <- applyRule rule q
        case substMaybe of
            Nothing -> return []
            Just (subst, qs') -> do
                res <- solve (Program rules) $ map (applySubst subst) (qs' ++ qs)
                return $ (`chainSubst` subst) <$> res

applyRule :: Rule -> Term -> State Int (Maybe (Subst, [Term]))
applyRule (Rule head tails) q = do
    maxVar <- get
    let (newHead : newTails) = map (addConstVar (maxVar + 1)) (head : tails)
    let substMaybe = unify newHead q
    case substMaybe of
        Left _ -> return Nothing
        Right subst -> do
            put $ max maxVar (maxVarOfList (newHead : newTails))
            return $ Just (subst, newTails)

parseFile :: String -> IO (Maybe Program)
parseFile path = do
    text <- readFile path
    let res = do
            program <- finishParser programP text
            return $ processProgram program
    return res

solveQuery :: Program -> String -> Maybe [String]
solveQuery p s = do
    qp <- finishParser queryP s
    let (Query ts vn) = processQuery qp
    let substs = evalState (solve p ts) (maxVarOfList ts)
    return $ map (\subst -> runReader (showSubst subst) vn) substs

consultFile :: String -> String -> IO (Maybe [String])
consultFile path q = (\mP -> mP >>= (`solveQuery` q)) <$> parseFile path

unifyStrings :: String -> String -> String
unifyStrings s1 s2 = fromMaybe "Parse error" $ do
    tp1 <- finishParser termP s1
    let t1 = processTerm tp1
    tp2 <- finishParser termP s2
    let t2 = processTerm tp2
    let (res, vn) = runState (unify <$> t1 <*> t2) emptyVarNames
    return $ runReader (showUnifyResult res) vn
