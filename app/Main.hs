module Main where

import Numeric
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapedChars <|> noneOf "\"")
    char '"'
    return $ String x

escapedChars :: Parser Char
escapedChars = do
    char '\\'
    c <- oneOf ['\\', '"', 'n', 'r', 't']
    return $ case c of
               '\\' -> c
               '"'  -> c
               'n'  -> '\n'
               'r'  -> '\r'
               't'  -> '\t'

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit


parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
