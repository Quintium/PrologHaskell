import Data.Char
import Data.Either

data Term  = Var Int | Function String [Term]
data Subst = Subst [Int] (Term -> Term)
data UnifyFailure = OccurFailure | ClashFailure 
data VarNames = VarNames (Int -> String) (String -> Int) Int

defaultVarNames :: VarNames
defaultVarNames = VarNames show (const (-1)) 0

showTerm :: VarNames -> Term -> String
showTerm (VarNames varToName nameToVar n) (Var v) = varToName v
showTerm vn (Function s []) = s
showTerm vn (Function s ts) =  s ++ "(" ++ concatMap ((++ ", ") . showTerm vn) (init ts) ++ showTerm vn (last ts) ++ ")"

showFailure :: UnifyFailure -> String
showFailure OccurFailure = "Occur failure"
showFailure ClashFailure = "Clash failure"

showSubst :: VarNames -> Subst -> String
showSubst (VarNames varToName nameToVar n) (Subst vs f) = concatMap (\v -> varToName v ++ " = " ++ showTerm (VarNames varToName nameToVar n) (f (Var v)) ++ "; ") vs

showUnifyResult :: VarNames -> Either UnifyFailure Subst -> String
showUnifyResult _ (Left failure) = showFailure failure
showUnifyResult vn (Right subst) = showSubst vn subst

unify :: Term -> Term -> Either UnifyFailure Subst
unify (Var v) t                           = varSubst v t
unify t (Var v)                           = varSubst v t
unify (Function s1 ts1) (Function s2 ts2) = fctSubst s1 ts1 s2 ts2

emptySubst :: Subst
emptySubst = Subst [] id

varEq :: Int -> Term -> Bool
varEq v (Var v2) = v == v2
varEq v _ = False

varSubst :: Int -> Term -> Either UnifyFailure Subst
varSubst v t | varEq v t  = Right emptySubst
             | occurs v t = Left OccurFailure
             | otherwise  = Right (Subst [v] s)
                                where s (Var n)           = if n == v then t else Var n
                                      s (Function str ts) = Function str (map s ts)

occurs :: Int -> Term -> Bool
occurs v1 (Var v2) = v1 == v2
occurs v (Function s ts) = any (occurs v) ts

fctSubst :: String -> [Term] -> String -> [Term] -> Either UnifyFailure Subst
fctSubst s1 ts1 s2 ts2 | s1 /= s2                 = Left ClashFailure
                       | length ts1 /= length ts2 = Left ClashFailure
                       | otherwise                = foldl substStep (Right emptySubst) (zip ts1 ts2)

substStep :: Either UnifyFailure Subst -> (Term, Term) -> Either UnifyFailure Subst
substStep unifyResult (t1, t2) = do Subst vs1 f1 <- unifyResult
                                    Subst vs2 f2 <- unify (f1 t1) (f1 t2)
                                    return $ Subst (vs1 ++ vs2) (f2 . f1)

emptyVarNames :: VarNames
emptyVarNames = VarNames (\n -> "Error: No variable with id " ++ show n) (const (-1)) 0

unifyStrings :: String -> String -> String
unifyStrings s1 s2 = showUnifyResult vn (unify t1 t2) 
                        where (t2, vn)  = parseTerm vn1 s2
                              (t1, vn1) = parseTerm emptyVarNames s1

parseTerm :: VarNames -> String -> (Term, VarNames)
parseTerm vn s | ' ' `elem` s = parseTerm vn (filter (/= ' ') s)
               | '(' `notElem` s = parseSingle vn s
               | otherwise = let (fnName, parts, _, _) = foldl parseStep ("", [], "", 0) s
                                 (terms, vnLast) = parseTerms vn parts
                             in (Function fnName terms, vnLast) 

parseStep :: (String, [String], String, Int) -> Char -> (String, [String], String, Int)
parseStep (fn, parts, s, 0) c        | c == '('  = (fn,        parts,        "",       1           )
                                     | otherwise = (fn ++ [c], parts,        s,        0           )
parseStep (fn, parts, s, 1) c        | c == '('  = (fn,        parts,        s ++ "(", 2           )
                                     | c == ')'  = (fn,        parts ++ [s], "",       0           )
                                     | c == ','  = (fn,        parts ++ [s], "",       1           )
                                     | otherwise = (fn,        parts,        s ++ [c], 1           )
parseStep (fn, parts, s, brackets) c | c == '('  = (fn,        parts,        s ++ "(", brackets + 1)
                                     | c == ')'  = (fn,        parts,        s ++ ")", brackets - 1)   
                                     | otherwise = (fn,        parts,        s ++ [c], brackets    )   

parseTerms :: VarNames -> [String] -> ([Term], VarNames)
parseTerms vn [] = ([], vn)
parseTerms vn (t:ts) = let (term, vn2) = parseTerm vn t
                           (terms, vnLast) = parseTerms vn2 ts
                       in (term:terms, vnLast)              

parseSingle :: VarNames -> String -> (Term, VarNames)
parseSingle (VarNames varToName nameToVar n) s | isUpper (head s) = parseVar (VarNames varToName nameToVar n) s
                                               | otherwise = parseAtom (VarNames varToName nameToVar n) s

parseVar :: VarNames -> String -> (Term, VarNames)
parseVar (VarNames varToName nameToVar n) s | nameToVar s == -1 = (Var n, VarNames (\x -> if x == n then s else varToName x) (\x -> if x == s then n else nameToVar x) (n+1))
                                            | otherwise         = (Var (nameToVar s), VarNames varToName nameToVar n)

parseAtom :: VarNames -> String -> (Term, VarNames)
parseAtom vn s = (Function s [], vn)