import Data.Char

data Term  = Var Int | Function String [Term]
data Subst = Subst [Int] (Term -> Term) | OccurFailure | ClashFailure 
data VarNames = VarNames (Int -> String) (String -> Int) Int

showTerm :: VarNames -> Term -> String
showTerm (VarNames varToName nameToVar n) (Var v) = varToName v
showTerm vn (Function s []) = s
showTerm vn (Function s ts) =  s ++ "(" ++ (concatMap ((++ ", ") . showTerm vn) (init ts)) ++ (showTerm vn (last ts)) ++ ")"

instance Show Term where
    show :: Term -> String
    show = showTerm (VarNames show (const (-1)) 0)

showSubst :: VarNames -> Subst -> String
showSubst (VarNames varToName nameToVar n) (Subst vs f) = concatMap (\v -> varToName v ++ " = " ++ showTerm (VarNames varToName nameToVar n) (f (Var v)) ++ "; ") vs
showSubst _ OccurFailure = "Occur Failure"
showSubst _ ClashFailure = "Clash Failure"

instance Show Subst where
    show :: Subst -> String
    show = showSubst (VarNames show (const (-1)) 0)

unify :: Term -> Term -> Subst
unify (Var v) t                           = varSubst v t
unify t (Var v)                           = varSubst v t
unify (Function s1 ts1) (Function s2 ts2) = fctSubst s1 ts1 s2 ts2

varEq :: Int -> Term -> Bool
varEq v (Var v2) = v == v2
varEq v _ = False

varSubst :: Int -> Term -> Subst
varSubst v t | varEq v t = emptySubst
             | occurs v t               = OccurFailure
             | otherwise                = Subst [v] s
                                            where s (Var n)           = if n == v then t else Var n
                                                  s (Function str ts) = Function str (map s ts)

occurs :: Int -> Term -> Bool
occurs v1 (Var v2) = v1 == v2
occurs v (Function s ts) = any (occurs v) ts

fctSubst :: String -> [Term] -> String -> [Term] -> Subst
fctSubst s1 ts1 s2 ts2 | s1 /= s2                 = ClashFailure
                       | length ts1 /= length ts2 = ClashFailure
                       | otherwise                = foldl substStep emptySubst (zip ts1 ts2)

substStep :: Subst -> (Term, Term) -> Subst
substStep OccurFailure _ = OccurFailure
substStep ClashFailure _ = ClashFailure
substStep (Subst vs f) (t1, t2) = chainSubst (Subst vs f) (unify (f t1) (f t2)) 

-- Substitutions to be chained should have disjunct variables
chainSubst :: Subst -> Subst -> Subst
chainSubst OccurFailure _                = OccurFailure
chainSubst ClashFailure _                = ClashFailure
chainSubst _ OccurFailure                = OccurFailure
chainSubst _ ClashFailure                = ClashFailure
chainSubst (Subst vs1 f1) (Subst vs2 f2) = Subst (vs1 ++ vs2) (f2 . f1)

emptySubst :: Subst
emptySubst = Subst [] id

emptyVarNames :: VarNames
emptyVarNames = VarNames (\n -> "Error: No variable with id " ++ show n) (const (-1)) 0

unifyStrings :: String -> String -> String
unifyStrings s1 s2 = showSubst vn (unify t1 t2) 
                        where (t2, vn)  = parseTerm vn1 s2
                              (t1, vn1) = parseTerm emptyVarNames s1

parseTerm :: VarNames -> String -> (Term, VarNames)
parseTerm vn s | ' ' `elem` s = parseTerm vn (filter (/= ' ') s)
               | '(' `notElem` s = parseSingle vn s
               | otherwise = stepResToParseRes vn (foldl parseStep ("", [], "", 0) s)

parseStep :: (String, [String], String, Int) -> Char -> (String, [String], String, Int)
parseStep (fn, parts, s, brackets) c | c == '('  = if brackets == 0 then (fn, parts, "", 1) else (fn, parts, s ++ "(", brackets + 1)
                                 | c == ')'  = if brackets == 1 then (fn, parts ++ [s], "", 0) else (fn, parts, s ++ ")", brackets - 1)
                                 | c == ','  = if brackets == 1 then (fn, parts ++ [s], "", 1) else (fn, parts, s ++ ",", brackets)
                                 | otherwise = if brackets == 0 then (fn ++ [c], parts, s, brackets) else (fn, parts, s ++ [c], brackets)

stepResToParseRes :: VarNames -> (String, [String], String, Int) -> (Term, VarNames)
stepResToParseRes vn (fn, parts, s, brackets) = (Function fn (map fst (tail convertRes)), snd (last convertRes))
                                                    where convertRes = scanl (\(prevVar, prevVn) termStr -> parseTerm prevVn termStr) (Var 0, vn) parts
                            
parseSingle :: VarNames -> String -> (Term, VarNames)
parseSingle (VarNames varToName nameToVar n) s | isUpper (head s) = parseVar (VarNames varToName nameToVar n) s
                                               | otherwise = parseAtom (VarNames varToName nameToVar n) s

parseVar :: VarNames -> String -> (Term, VarNames)
parseVar (VarNames varToName nameToVar n) s | nameToVar s == -1 = (Var n, VarNames (\x -> if x == n then s else varToName x) (\x -> if x == s then n else nameToVar x) (n+1))
                                            | otherwise         = (Var (nameToVar s), VarNames varToName nameToVar n)

parseAtom :: VarNames -> String -> (Term, VarNames)
parseAtom vn s = (Function s [], vn)