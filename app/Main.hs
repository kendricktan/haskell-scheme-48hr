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
             | Char Char
             | Bool Bool deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
    char '\"'
    x <- many (escapedChars <|> noneOf "\"")
    char '\"'
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

parseChar :: Parser LispVal
parseChar = do
    char '\''
    x <- count 1 anyChar
    char '\''
    return $ Char (head x)

parseNumber :: Parser LispVal
parseNumber = parseHex <|> parseOctal <|> parseDecimal

parseDecimal :: Parser LispVal
parseDecimal = (Number . read) <$> many1 digit

parseOctal :: Parser LispVal
parseOctal = do
    char 'o'
    (Number . fst . head . readOct) <$> many1 octDigit

parseHex :: Parser LispVal
parseHex = do
    char 'x'
    (Number . fst . head . readHex) <$> many1 hexDigit

parseExpr :: Parser LispVal
parseExpr = parseNumber
    <|> parseChar
    <|> parseString
    <|> parseAtom

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
