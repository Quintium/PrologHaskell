import Data.Char
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Control.Monad.Trans.Maybe
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
substStep unifyResult (t1, t2) = do s1 <- unifyResult
                                    s2 <- unify (applySubst s1 t1) (applySubst s1 t2)
                                    return $ chainSubst s1 s2

data TermP = FunctionP String [TermP] deriving Show

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser p'
        where p' s = do (x1, rest1) <- p s
                        runParser (f x1) rest1

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap = liftM

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser (\s -> Just (x, s))

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    p1 <*> p2 = do f <- p1
                   f <$> p2

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) = Parser p'
        where p' s = p1 s <|> p2 s

finishParser :: Parser a -> String -> Maybe a
finishParser p s = do (x, rest) <- runParser p s
                      guard (null rest)
                      return x

oneP :: (Char -> Bool) -> Parser Char
oneP f = Parser p
    where p (c:rest) | f c = Just (c, rest)
          p _              = Nothing

manyP :: (Char -> Bool) -> Parser String
manyP f = many (oneP f)

someP :: (Char -> Bool) -> Parser String
someP f = some (oneP f)

charP :: Char -> Parser Char
charP c = oneP (== c)

stringP :: String -> Parser String
stringP = mapM charP

spaceP :: Parser String
spaceP = manyP isSpace

expressionP :: Parser String
expressionP = someP (`notElem` "(),.:- \n")

atomP :: Parser TermP
atomP = (`FunctionP` []) <$> expressionP

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy delim p = ((:) <$> p <*> many (delim *> p)) <|> pure []

functionP :: Parser TermP
functionP = FunctionP <$> (expressionP <* spaceP <* charP '(' <* spaceP) <*> sepBy (charP ',') termP <* charP ')'

termP :: Parser TermP
termP = spaceP *> (functionP <|> atomP) <* spaceP

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
    tp <- finishParser termP s
    return $ processTerm tp

unifyStrings :: String -> String -> String
unifyStrings s1 s2 = fromMaybe "Parse error" $ do
    t1 <- parseTerm s1
    t2 <- parseTerm s2
    let (res, vn) = runState (unify <$> t1 <*> t2) (VarNames [])
    return $ runReader (showUnifyResult res) vn

data Program = Program [Rule] deriving Show
data Rule = Rule Term [Term] deriving Show
data Query = Query [Term] VarNames deriving Show

data ProgramP = ProgramP [RuleP] deriving Show
data RuleP = RuleP TermP [TermP] deriving Show
data QueryP = QueryP [TermP] deriving Show

factP :: Parser RuleP
factP = (`RuleP` []) <$> termP <* charP '.'

ruleP :: Parser RuleP
ruleP = RuleP <$> (termP <* stringP ":-" <* spaceP) <*> sepBy (charP ',') termP <* charP '.'

programP :: Parser ProgramP
programP = ProgramP <$> sepBy spaceP (ruleP <|> factP)

queryP :: Parser QueryP
queryP = spaceP *> (QueryP <$> sepBy (charP ',') termP) <* spaceP

processProgram :: ProgramP -> Program
processProgram (ProgramP rules) = Program $ map processRule rules

processRule :: RuleP -> Rule
processRule (RuleP head tails) = evalState (Rule <$> processTerm head <*> mapM processTerm tails) (VarNames [])

processQuery :: QueryP -> Query
processQuery (QueryP ts) = let (terms, vn) = runState (mapM processTerm ts) (VarNames []) 
                           in Query terms vn

chainSubst :: Subst -> Subst -> Subst
chainSubst (Subst vs1 f1) (Subst vs2 f2) = Subst (vs1 ++ vs2) (f1 . f2)

solve :: Program -> [Term] -> State Int [Subst]
solve _ [] = return [emptySubst]
solve (Program rules) (q:qs) = concat <$> mapM solveRule rules
    where solveRule rule = do substMaybe <- applyRule rule q
                              case substMaybe of
                                  Nothing -> return []
                                  Just (subst, qs') -> do res <- solve (Program rules) $ map (applySubst subst) (qs' ++ qs)
                                                          return $ (`chainSubst` subst) <$> res

applySubst :: Subst -> Term -> Term
applySubst (Subst _ f) = f
                                                                                                                  
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

maxVarOf :: Term -> Int
maxVarOf (Var n) = n
maxVarOf (Function _ ts) = maxVarOfList ts

maxVarOfList :: [Term] -> Int
maxVarOfList [] = -1
maxVarOfList ts = maximum $ map maxVarOf ts

addConstVar :: Int -> Term -> Term
addConstVar c (Var n) = Var (n + c)
addConstVar c (Function s ts) = Function s (map (addConstVar c) ts)

applyRule :: Rule -> Term -> State Int (Maybe (Subst, [Term]))
applyRule (Rule head tails) q = do 
    maxVar <- get
    let (newHead:newTails) = map (addConstVar (maxVar + 1)) (head:tails)
    let substMaybe = unify newHead q
    case substMaybe of
        Left _ -> return Nothing
        Right subst -> do put $ maxVarOfList (newHead:newTails)
                          return $ Just (subst, newTails)

parseFile :: String -> IO (Maybe Program)
parseFile path = do text <- readFile path
                    let res = do program <- finishParser programP text
                                 return $ processProgram program
                    return res

solveQuery :: Program -> String -> Maybe [String]
solveQuery p s = do qp <- finishParser queryP s
                    let (Query ts vn) = processQuery qp
                    let substs = evalState (solve p ts) (maxVarOfList ts)
                    return $ map (\subst -> runReader (showSubst subst) defaultVarNames) substs
                    
consultFile :: String -> String -> IO (Maybe [String])
consultFile path q = (\mP -> mP >>= (`solveQuery` q)) <$> parseFile path