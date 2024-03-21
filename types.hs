module Types where

import Data.List
import Control.Monad.Reader

data VarNames     = VarNames [String] deriving Show

data Term         = Var Int | Function String [Term] deriving Show
data Program = Program [Rule] deriving Show
data Rule = Rule Term [Term] deriving Show
data Query = Query [Term] VarNames deriving Show

data TermP = FunctionP String [TermP] deriving Show
data ProgramP = ProgramP [RuleP] deriving Show
data RuleP = RuleP TermP [TermP] deriving Show
data QueryP = QueryP [TermP] deriving Show

data Subst        = Subst [Int] (Term -> Term)
data UnifyFailure = OccurFailure | ClashFailure

emptyVarNames :: VarNames
emptyVarNames = VarNames []

defaultVarNames :: VarNames
defaultVarNames = VarNames $ map show [0..1000]

emptySubst :: Subst
emptySubst = Subst [] id

showTerm :: Term -> Reader VarNames String
showTerm (Var v)         = do VarNames names <- ask
                              if v < length names
                                then return $ names !! v
                                else return $ "C" ++ show v
                              
showTerm (Function s []) = return s
showTerm (Function s ts) = do ts' <- mapM showTerm ts
                              return $ s ++ "(" ++ intercalate ", " ts' ++ ")"

showFailure :: UnifyFailure -> String
showFailure OccurFailure = "Occur failure"
showFailure ClashFailure = "Clash failure"

showSubst :: Subst -> Reader VarNames String
showSubst (Subst vs f) = do VarNames names <- ask
                            let namedVs = filter (< length names) vs
                            if null namedVs
                                then return "true"
                                else do ts <- mapM (showTerm . f . Var) namedVs
                                        return $ intercalate "; " (zipWith (\v t -> names !! v ++ " = " ++ t) namedVs ts)
                                

showUnifyResult :: Either UnifyFailure Subst -> Reader VarNames String
showUnifyResult (Left failure) = return (showFailure failure)
showUnifyResult (Right subst) = showSubst subst