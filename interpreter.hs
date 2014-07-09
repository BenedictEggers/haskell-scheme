-- Ben Eggers
-- MIT Licensed

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "Error: " ++ show err
    Right _ -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))