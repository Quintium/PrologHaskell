module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser p'
      where
        p' s = do
            (x1, rest1) <- p s
            runParser (f x1) rest1

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap = liftM

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser (\s -> Just (x, s))

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    p1 <*> p2 = do
        f <- p1
        f <$> p2

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) = Parser p'
      where
        p' s = p1 s <|> p2 s

finishParser :: Parser a -> String -> Maybe a
finishParser p s = do
    (x, rest) <- runParser p s
    guard (null rest)
    return x

oneP :: (Char -> Bool) -> Parser Char
oneP f = Parser p
  where
    p (c : rest) | f c = Just (c, rest)
    p _ = Nothing

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

expressionP :: [Char] -> Parser String
expressionP illegalChars = someP (\c -> c `notElem` illegalChars && not (isSpace c))

-- parses list of terms (second argument) separated by a delimiter (first argument)
-- third argument are also counted as terms, but they are ignored (such as comments)
sepByP :: Parser a -> Parser b -> Parser c -> Parser [b]
sepByP delim term ignore =
    ( catMaybes
        <$> ( do
                single <- Just <$> term
                multiple <- many (delim *> ((Nothing <$ ignore) <|> (Just <$> term)))
                return $ single : multiple
            )
    )
        <|> pure []

untilP :: String -> Parser String
untilP end = Parser p
  where
    p s =
        Just $
            fromMaybe
                (s, "")
                ( do
                    endStart <- findIndex (end `isPrefixOf`) (tails s)
                    return $ splitAt endStart s
                )

failP :: Parser a
failP = Parser p
  where
    p s = Nothing
