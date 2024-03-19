import Data.Char
import Data.Either
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad.State

data Term         = Var Int | Function String [Term] deriving Show
data Subst        = Subst [Int] (Term -> Term)
data UnifyFailure = OccurFailure | ClashFailure
data VarNames     = VarNames [String] deriving Show

defaultVarNames :: VarNames
defaultVarNames = VarNames $ map show [0..100]

showTerm :: Term -> Reader VarNames String
showTerm (Var v)         = do VarNames names <- ask
                              return $ names !! v
showTerm (Function s []) = return s
showTerm (Function s ts) = do ts' <- mapM showTerm ts
                              return $ s ++ "(" ++ intercalate ", " ts' ++ ")"

showFailure :: UnifyFailure -> String
showFailure OccurFailure = "Occur failure"
showFailure ClashFailure = "Clash failure"

showSubst :: Subst -> Reader VarNames String
showSubst (Subst vs f) = do VarNames names <- ask
                            ts <- mapM (showTerm . f . Var) vs
                            return $ intercalate "; " (zipWith (\v t -> names !! v ++ " = " ++ t) vs ts)

showUnifyResult :: Either UnifyFailure Subst -> Reader VarNames String
showUnifyResult (Left failure) = return (showFailure failure)
showUnifyResult (Right subst) = showSubst subst

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

data TermP = FunctionP String [TermP] deriving Show

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser p' 
        where p' s = do (x, rest) <- p s
                        return (f x, rest)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser (\s -> Just (x, s))

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser p1) <*> (Parser p2) = Parser p'
        where p' s = do (x1, rest1) <- p1 s
                        (x2, rest2) <- p2 rest1
                        return (x1 x2, rest2)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) = Parser p'
        where p' s = case (p1 s, p2 s) of
                          (Just (x1, r1), Just (x2, r2)) -> if length r1 <= length r2 then p1 s else p2 s
                          (x1, x2) -> x1 <|> x2

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser p'
        where p' s = do (x1, rest1) <- p s
                        runParser (f x1) rest1

oneP :: (Char -> Bool) -> Parser Char
oneP f = Parser p
    where p (c:rest) | f c = Just (c, rest)
          p _              = Nothing

manyP :: (Char -> Bool) -> Parser String
manyP f = many (oneP f)

guarantee :: Parser a -> Parser a
guarantee p = Parser p'
    where p' s = do (x, _) <- runParser p s
                    return (x, s)

someP :: (Char -> Bool) -> Parser String
someP f = some (oneP f)

charP :: Char -> Parser Char
charP c = oneP (== c)

stringP :: String -> Parser String
stringP = mapM charP

spaceP :: Parser String
spaceP = manyP isSpace

expressionP :: Parser String
expressionP = someP (`notElem` "(), ")

atomP :: Parser TermP
atomP = (\s -> FunctionP s []) <$> expressionP

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy delim p = ((:) <$> p <*> many (delim *> p)) <|> pure []

functionP :: Parser TermP
functionP = FunctionP <$> (expressionP <* spaceP <* charP '(' <* spaceP) <*> sepBy (charP ',') termP <* charP ')'

termP :: Parser TermP
termP = spaceP *> (atomP <|> functionP) <* spaceP

processTerm :: TermP -> State VarNames Term
processTerm (FunctionP name []) | isUpper (head name) = do 
    (VarNames vn) <- get
    if name `elem` vn
        then return $ Var (fromJust (elemIndex name vn))
        else do put (VarNames (vn ++ [name]))
                return $ Var (length vn)
                              | otherwise = return $ Function name []
processTerm (FunctionP name args) = do 
    args' <- mapM processTerm args
    return $ Function name args'

parseTerm :: String -> Maybe (State VarNames Term)
parseTerm s = do
    (tp, "") <- runParser termP s -- fails if string not completely parsed
    return $ processTerm tp

unifyStrings :: String -> String -> String
unifyStrings s1 s2 = let res = do
                            t1 <- parseTerm s1
                            t2 <- parseTerm s2
                            return (t1, t2)
                     in f res

f :: Maybe (State VarNames Term, State VarNames Term) -> String
f Nothing = "Parse error"
f (Just (t1, t2)) = runReader (showUnifyResult res) vn 
    where (res, vn) = runState (do
            t1' <- t1
            t2' <- t2
            return $ unify t1' t2') (VarNames [])