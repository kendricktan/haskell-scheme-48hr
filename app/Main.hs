module Main where

import Numeric
import Control.Monad
import Data.Complex
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Float Float
             | Complex (Complex Float)
             | Bool Bool deriving Show

spaces :: Parser ()
spaces = skipMany1 space

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
parseNumber = parseComplex
          <|> try parseFloat
          <|> try parseHex
          <|> try parseOctal
          <|> try parseDecimal

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

parseComplex :: Parser LispVal
parseComplex = do
    rn <- fmap toDouble (try parseFloat <|> parseDecimal)
    char '+'
    cn <- fmap toDouble (try parseFloat <|> parseDecimal)
    char 'i'
    return $ Complex (rn :+ cn)
        where toDouble (Float x) = x
              toDouble (Number x) = fromIntegral x

parseFloat :: Parser LispVal
parseFloat = do
    i <- many1 digit
    p <- char '.'
    d <- many1 digit
    let fn = i ++ [p] ++ d
    return $ (Float . read) fn

parseList :: Parser LispVal
parseList = char '(' >> parseList1

parseList1 :: Parser LispVal
parseList1 = (char ')' >> (return . List) [])
                <|> do
                    expr <- parseExpr
                    parseList2 [expr]

parseList2 :: [LispVal] -> Parser LispVal
parseList2 expr = (char ')' >> (return . List) (reverse expr))
                <|> (spaces >> parseList3 expr)

parseList3 :: [LispVal] -> Parser LispVal
parseList3 expr = do char '.' >> spaces
                     dotted <- parseExpr
                     char ')'
                     return $ DottedList expr dotted
                  <|> do
                      next <- parseExpr
                      parseList2 (next:expr)

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
    string ",@"
    x <- parseExpr
    return $ List [Atom "unquote-splicing", x]

parseQuotes :: Parser LispVal
parseQuotes = try parseQuoted
          <|> try parseQuasiquoted
          <|> try parseUnquoted
          <|> try parseUnquoteSplicing

parseExpr :: Parser LispVal
parseExpr = try parseAtom
    <|> try parseString
    <|> try parseChar
    <|> try parseQuotes
    <|> try parseNumber
    <|> do char '('
           x <- try parseList <|> parseDottedList
           char ')'
           return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
