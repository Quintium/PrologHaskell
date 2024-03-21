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
    return $ chainSubst s2 s1

solve :: Int -> Program -> [Term] -> [Subst]
solve _ _ [] = [emptySubst]
solve maxVar (Program rules) (q : qs) = concatMap (\r -> applyRule maxVar r q (Program rules) qs) rules

applyRule :: Int -> Rule -> Term -> Program -> [Term] -> [Subst]
applyRule maxVar (Rule head tails) q p qs =
    let (newHead : newTails) = map (addConstVar (maxVar + 1)) (head : tails)
        substMaybe = unify newHead q
     in case substMaybe of
            Left _ -> []
            Right subst ->
                let newMax = max maxVar (maxVarOfList (newHead : newTails))
                 in (`chainSubst` subst) <$> solve newMax p (map (applySubst subst) (newTails ++ qs))

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
    let substs = solve (maxVarOfList ts) p ts
    return $ map (\subst -> runReader (showSubst subst) vn) substs

consultFile :: String -> String -> IO (Maybe [String])
consultFile path q = do file <- parseFile path
                        return $ file >>= (`solveQuery` q)

unifyStrings :: String -> String -> String
unifyStrings s1 s2 = fromMaybe "Parse error" $ do
    tp1 <- finishParser termP s1
    tp2 <- finishParser termP s2
    let ((t1, t2), vn) = runState ((,) <$> processTerm tp1 <*> processTerm tp2) emptyVarNames
    return $ runReader (showUnifyResult (unify t1 t2)) vn
