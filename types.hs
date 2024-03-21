module Types where

import Data.List
import Control.Monad.Reader

data VarNames
    = VarNames [String]
    deriving (Show)

data Term
    = Var Int
    | Function String [Term]
    deriving (Show)

data Program
    = Program [Rule]
    deriving (Show)

data Rule
    = Rule Term [Term]
    deriving (Show)

data Query
    = Query [Term] VarNames
    deriving (Show)

data TermP
    = FunctionP String [TermP]
    deriving (Show)

data ProgramP
    = ProgramP [RuleP]
    deriving (Show)

data RuleP
    = RuleP TermP [TermP]
    deriving (Show)

data QueryP
    = QueryP [TermP]
    deriving (Show)

data Subst
    = Subst [Int] (Term -> Term)

data UnifyFailure
    = OccurFailure
    | ClashFailure

varEq :: Int -> Term -> Bool
varEq v (Var v2) = v == v2
varEq v _ = False

emptyVarNames :: VarNames
emptyVarNames = VarNames []

defaultVarNames :: VarNames
defaultVarNames = VarNames $ map show [0 .. 1000]

emptySubst :: Subst
emptySubst = Subst [] id

showTerm :: Term -> Reader VarNames String
showTerm (Var v) = do
    VarNames names <- ask
    if v < length names
        then return $ names !! v
        else return $ "C" ++ show v
showTerm (Function s []) = return s
showTerm (Function s ts) = do
    ts' <- mapM showTerm ts
    return $ s ++ "(" ++ intercalate ", " ts' ++ ")"

showFailure :: UnifyFailure -> String
showFailure OccurFailure = "Occur failure"
showFailure ClashFailure = "Clash failure"

showSubst :: Subst -> Reader VarNames String
showSubst (Subst vs f) = do
    VarNames names <- ask
    let namedVs = filter (< length names) vs
    if null namedVs
        then return "true"
        else do
            ts <- mapM (showTerm . f . Var) namedVs
            return $
                intercalate
                    "; "
                    (zipWith (\v t -> names !! v ++ " = " ++ t) namedVs ts)

showUnifyResult :: Either UnifyFailure Subst -> Reader VarNames String
showUnifyResult (Left failure) = return (showFailure failure)
showUnifyResult (Right subst) = showSubst subst

applySubst :: Subst -> Term -> Term
applySubst (Subst _ f) = f

-- assumes that substitutions are disjunct
chainSubst :: Subst -> Subst -> Subst
chainSubst (Subst vs1 f1) (Subst vs2 f2) = Subst (vs1 ++ vs2) (f1 . f2)

maxVarOf :: Term -> Int
maxVarOf (Var n) = n
maxVarOf (Function _ ts) = maxVarOfList ts

maxVarOfList :: [Term] -> Int
maxVarOfList [] = -1
maxVarOfList ts = maximum $ map maxVarOf ts

addConstVar :: Int -> Term -> Term
addConstVar c (Var n) = Var (n + c)
addConstVar c (Function s ts) = Function s (map (addConstVar c) ts)
