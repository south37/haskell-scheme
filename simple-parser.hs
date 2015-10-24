import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Data.Char
import Numeric

symbol :: Parser Char
symbol = oneOf  "!#$%&|*+-/:<=>?@^_~"

escapedChars :: Parser Char
escapedChars = do x <- char '\\' >> (oneOf "nrt\\\"")
                  return $ case x of
                             'n' -> '\n'
                             'r' -> '\r'
                             't' -> '\t'
                             _   -> x

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"" <|> escapedChars)
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do prefix <- ((char '#' >> oneOf "box") <|> digit)
                 case prefix of
                   'b' -> do numberStr <- many1 $ oneOf "01"
                             return $ Number (parseBin numberStr)
                   'o' -> do numberStr <- many1 $ oneOf "01234567"
                             return $ Number (extractNumber $ readOct numberStr)
                   'x' -> do numberStr <- many1 $ oneOf "0123456789abcde"
                             return $ Number (extractNumber $ readHex numberStr)
                   _   -> do numberStr <- many1 digit
                             return $ Number (read $ prefix:numberStr)

extractNumber :: [(a, String)] -> a
extractNumber ((number, _):_) = number

parseBin :: String -> Integer
parseBin input = foldl (\x y -> x * 2 + y) 0 $ map (\x -> read [x]) input

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found valud"

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
