module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char (isLower, isUpper, isDigit, isAlpha)

import Lambda
import Binding
import Distribution.PackageDescription (parsecLibraryNameComponent)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input ->
        case p input of
            Just (result, rest) -> Just (f result, rest)
            Nothing             -> Nothing

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser pf) <*> (Parser pa) = Parser $ \input ->
        case pf input of
            Just (f, rest) -> case pa rest of
                Just (a, rest') -> Just (f a, rest')
                Nothing -> Nothing
            Nothing -> Nothing

instance Monad Parser where
    return = pure
    (Parser pa) >>= f = Parser $ \input ->
        case pa input of
            Just (a, rest) -> parse (f a) rest
            Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        p1 input <|> p2 input

-- Parser combinator functions
char :: Char -> Parser Char
char c = satisfy (== c)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input -> case input of
    (x:xs) | predicate x -> Just (x, xs)
    _ -> Nothing

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> many p


eof :: Parser ()
eof = Parser $ \input -> if null input then Just ((), "") else Nothing

-- Parsing a variable name
parseVarName :: Parser String
parseVarName = oneOrMore (satisfy isLower)

-- Parsing a variable
parseVar :: Parser Lambda
parseVar = Var <$> parseVarName

-- Parsing a macro name
parseMacroName :: Parser String
parseMacroName = oneOrMore (satisfy (\c -> isUpper c || isDigit c))

-- Parsing a macro
parseMacro :: Parser Lambda
parseMacro = Macro <$> parseMacroName

-- Parsing a lambda abstraction
parseAbs :: Parser Lambda
parseAbs = do
    char '\\'       -- caracterul back-slash `\` trebuie escapat
    var <- parseVarName
    char '.'
    Abs var <$> parseLambda'

-- Parsing a lambda application
parseApp :: Parser Lambda
parseApp = do
    char '('
    e1 <- parseLambda'
    spaces
    e2 <- parseLambda'
    char ')'
    return $ (App e1 e2)

-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda input = case parse (parseLambda' <* eof) input of
    Just (lambda, _) -> lambda
    Nothing          -> error "Parsing failed."

-- Parsing any lambda expression
parseLambda' :: Parser Lambda
parseLambda' = parseAbs <|> parseApp <|> parseVar <|> parseMacro

-- Skipping spaces
spaces :: Parser ()
-- poate fi rescrisa: spaces = void (many (satisfy (\c -> c `elem` [' ', '\n', '\t'])))
spaces = void $ many $ satisfy (\c -> c == ' ' || c == '\n' || c == '\t')


-- 3.3. Parsing a line
parseLine :: String -> Either String Line
parseLine input = case parse (parseLine' <* eof) input of
    Just (line, _) -> Right line
    Nothing        -> Left "Parsing failed."

parseLine' :: Parser Line
parseLine' = parseBinding <|> parseEval

parseBinding :: Parser Line
parseBinding = do
    name <- parseMacroName
    spaces
    char '='
    spaces
    Binding name <$> parseLambda'

parseEval :: Parser Line
parseEval = Eval <$> parseLambda'
