import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Data.Char
import Numeric

symbol :: Parser Char
symbol = oneOf  "!$%&|*+-/:<=>?@^_~"

escapedChars :: Parser Char
escapedChars = do x <- char '\\' >> (oneOf "nrt\\\"")
                  return $ case x of
                             'n' -> '\n'
                             'r' -> '\r'
                             't' -> '\t'
                             _   -> x

spetialChars :: Parser String
spetialChars = try (string "altmode")   <|>
               try (string "backnext")  <|>
               try (string "backspace") <|>
               try (string "call")      <|>
               try (string "linefeed")  <|>
               try (string "page")      <|>
               try (string "return")    <|>
               try (string "rubout")    <|>
               try (string "space")     <|>
               try (string "tab")       <|>
               try (string "NUL")       <|>
               try (string "SOH")       <|>
               try (string "STX")       <|>
               try (string "ETX")       <|>
               try (string "EOT")       <|>
               try (string "ENQ")       <|>
               try (string "ACK")       <|>
               try (string "BEL")       <|>
               try (string "BS")        <|>
               try (string "HT")        <|>
               try (string "LF")        <|>
               try (string "VT")        <|>
               try (string "FF")        <|>
               try (string "CR")        <|>
               try (string "SO")        <|>
               try (string "SI")        <|>
               try (string "DLE")       <|>
               try (string "DC1")       <|>
               try (string "DC2")       <|>
               try (string "DC3")       <|>
               try (string "DC4")       <|>
               try (string "NAK")       <|>
               try (string "SYN")       <|>
               try (string "ETB")       <|>
               try (string "CAN")       <|>
               try (string "EM")        <|>
               try (string "SUB")       <|>
               try (string "ESC")       <|>
               try (string "FS")        <|>
               try (string "GS")        <|>
               try (string "RS")        <|>
               try (string "US")        <|>
               try (string "DEL")

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               return $ Atom (first:rest)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedChars <|> noneOf "\"")
                 char '"'
                 return $ String x

parseNumber :: Parser LispVal
parseNumber = do prefix <- (try $ char '#' >> oneOf "box") <|> digit
                 case prefix of
                   'b' -> do numberStr <- many1 $ oneOf "01"
                             return $ Number (bin2dig numberStr)
                   'o' -> do numberStr <- many1 $ oneOf "01234567"
                             return $ Number (fst $ (readOct numberStr) !! 0)
                   'x' -> do numberStr <- many1 $ oneOf "0123456789abcde"
                             return $ Number (fst $ (readHex numberStr) !! 0)
                   _   -> do numberStr <- many1 digit
                             return $ Number (read $ prefix:numberStr)

parseChar :: Parser LispVal
parseChar = do
    try $ string "#\\"
    value <- spetialChars
             <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
    return $ Char $ case value of
                      "altmode"   -> '\ESC'
                      "backnext"  -> '\US'
                      "backspace" -> '\BS'
                      "call"      -> '\SUB'
                      "linefeed"  -> '\LF'
                      "page"      -> '\FF'
                      "return"    -> '\CR'
                      "rubout"    -> '\DEL'
                      "space"     -> ' '
                      "tab"       -> '\HT'
                      "NUL"       -> '\NUL'
                      "SOH"       -> '\SOH'
                      "STX"       -> '\STX'
                      "ETX"       -> '\ETX'
                      "EOT"       -> '\EOT'
                      "ENQ"       -> '\ENQ'
                      "ACK"       -> '\ACK'
                      "BEL"       -> '\BEL'
                      "BS"        -> '\BS'
                      "HT"        -> '\HT'
                      "LF"        -> '\LF'
                      "VT"        -> '\VT'
                      "FF"        -> '\FF'
                      "CR"        -> '\CR'
                      "SO"        -> '\SO'
                      "SI"        -> '\SI'
                      "DLE"       -> '\DLE'
                      "DC1"       -> '\DC1'
                      "DC2"       -> '\DC2'
                      "DC3"       -> '\DC3'
                      "DC4"       -> '\DC4'
                      "NAK"       -> '\NAK'
                      "SYN"       -> '\SYN'
                      "ETB"       -> '\ETB'
                      "CAN"       -> '\CAN'
                      "EM"        -> '\EM'
                      "SUB"       -> '\SUB'
                      "ESC"       -> '\ESC'
                      "FS"        -> '\FS'
                      "GS"        -> '\GS'
                      "RS"        -> '\RS'
                      "US"        -> '\US'
                      "DEL"       -> '\DEL'
                      otherwize   -> (value !! 0)

parseBool :: Parser LispVal
parseBool = do x <- try $ char '#' >> oneOf "tf"
               case x of
                 't' -> return $ Bool True
                 'f' -> return $ Bool False

bin2dig :: String -> Integer
bin2dig input = foldl (\x y -> x * 2 + y) 0 $ map (\x -> read [x]) input

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseChar
        <|> parseBool

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found valud"

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
