import Data.Char
import Data.Either
import Data.List
import Data.Maybe

data Term         = Var Int | Function String [Term] deriving Show
data Subst        = Subst [Int] (Term -> Term)
data UnifyFailure = OccurFailure | ClashFailure
data VarNames     = VarNames [String] deriving Show

defaultVarNames :: VarNames
defaultVarNames = VarNames $ map show [0..100]

showTerm :: VarNames -> Term -> String
showTerm (VarNames names) (Var v) = names !! v
showTerm vn (Function s [])       = s
showTerm vn (Function s ts)       = s ++ "(" ++ concatMap ((++ ", ") . showTerm vn) (init ts) ++ showTerm vn (last ts) ++ ")"

showFailure :: UnifyFailure -> String
showFailure OccurFailure = "Occur failure"
showFailure ClashFailure = "Clash failure"

showSubst :: VarNames -> Subst -> String
showSubst (VarNames names) (Subst vs f) = concatMap (\v -> names !! v ++ " = " ++ showTerm (VarNames names) (f (Var v)) ++ "; ") vs

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
varEq v _        = False

varSubst :: Int -> Term -> Either UnifyFailure Subst
varSubst v t | varEq v t  = Right emptySubst
             | occurs v t = Left OccurFailure
             | otherwise  = Right (Subst [v] s)
                                where s (Var n)           = if n == v then t else Var n
                                      s (Function str ts) = Function str (map s ts)

occurs :: Int -> Term -> Bool
occurs v1 (Var v2)       = v1 == v2
occurs v (Function s ts) = any (occurs v) ts

fctSubst :: String -> [Term] -> String -> [Term] -> Either UnifyFailure Subst
fctSubst s1 ts1 s2 ts2 | s1 /= s2                 = Left ClashFailure
                       | length ts1 /= length ts2 = Left ClashFailure
                       | otherwise                = foldl substStep (Right emptySubst) (zip ts1 ts2)

substStep :: Either UnifyFailure Subst -> (Term, Term) -> Either UnifyFailure Subst
substStep unifyResult (t1, t2) = do Subst vs1 f1 <- unifyResult
                                    Subst vs2 f2 <- unify (f1 t1) (f1 t2)
                                    return $ Subst (vs1 ++ vs2) (f2 . f1)

unifyStrings :: String -> String -> String
unifyStrings s1 s2 = let (t1, vn1) = parseTerm (VarNames []) s1
                         (t2, vn2) = parseTerm vn1 s2
                     in showUnifyResult vn2 (unify t1 t2) 

parseTerm :: VarNames -> String -> (Term, VarNames)
parseTerm vn s | ' ' `elem` s    = parseTerm vn (filter (/= ' ') s)
               | '(' `notElem` s = parseSingle vn s
               | otherwise       = let (fnName, parts, _, _) = foldl parseStep ("", [], "", 0) s
                                       (terms, vnLast) = parseChain parseTerm vn parts
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

-- chain multiple parses by using the varnames of the previous element
parseChain :: (VarNames -> String -> (a, VarNames)) -> VarNames -> [String] -> ([a], VarNames)
parseChain parse vn []     = ([], vn)
parseChain parse vn (x:xs) = let (first, vn2)     = parse vn x
                                 (others, vnLast) = parseChain parse vn2 xs
                             in (first:others, vnLast)             

parseSingle :: VarNames -> String -> (Term, VarNames)
parseSingle vn s | isUpper (head s) = parseVar vn s
                 | otherwise        = (Function s [], vn)

parseVar :: VarNames -> String -> (Term, VarNames)
parseVar (VarNames names) s | s `elem` names = (Var $ fromJust (elemIndex s names), VarNames names)
                            | otherwise      = (Var (length names), VarNames (names ++ [s]))

data Knowledge = Knowledge [Rule] deriving Show
data Rule = Rule Literal [Literal] VarNames deriving Show
data Literal = TrueLiteral | Literal Term deriving Show

parseProgram :: [String] -> Knowledge
parseProgram lines =  Knowledge $ map parseRule lines

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delim [] = [[]]
splitOn delim (x:xs) | delim`isPrefixOf` (x:xs) = [] : splitOn delim (drop (length delim) (x:xs))
                     | otherwise = let (first:rest) = splitOn delim xs
                                   in (x:first):rest

parseRule :: String -> Rule
parseRule s | ":-" `isInfixOf` s = let (effect:cause:_)              = splitOn ":-" (init s)
                                       causes = splitOn "," cause
                                       (effectTerm:causeTerms, vn) = parseChain parseTerm (VarNames []) (effect:causes)
                                   in Rule (Literal effectTerm) (map Literal causeTerms) vn
            | otherwise          = let (term, vn) = parseTerm (VarNames []) (init s)
                                   in Rule (Literal term) [TrueLiteral] vn

