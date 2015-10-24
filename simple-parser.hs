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
                 x <- many (noneOf "\"" <|> escapedChars)
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
parseChar = parseNormalChar <|> parseSpecialChar

parseNormalChar :: Parser LispVal
parseNormalChar = do try $ string "#\\"
                     x <- letter
                     return $ Char x

parseSpecialChar :: Parser LispVal
parseSpecialChar = do try $ string "#\\"
                      x <- spetialChars
                      case x of
                        "altmode"   -> return $ Char '\ESC'
                        "backnext"  -> return $ Char '\US'
                        "backspace" -> return $ Char '\BS'
                        "call"      -> return $ Char '\SUB'
                        "linefeed"  -> return $ Char '\LF'
                        "page"      -> return $ Char '\FF'
                        "return"    -> return $ Char '\CR'
                        "rubout"    -> return $ Char '\DEL'
                        "space"     -> return $ Char ' '
                        "tab"       -> return $ Char '\HT'
                        "NUL"       -> return $ Char '\NUL'
                        "SOH"       -> return $ Char '\SOH'
                        "STX"       -> return $ Char '\STX'
                        "ETX"       -> return $ Char '\ETX'
                        "EOT"       -> return $ Char '\EOT'
                        "ENQ"       -> return $ Char '\ENQ'
                        "ACK"       -> return $ Char '\ACK'
                        "BEL"       -> return $ Char '\BEL'
                        "BS"        -> return $ Char '\BS'
                        "HT"        -> return $ Char '\HT'
                        "LF"        -> return $ Char '\LF'
                        "VT"        -> return $ Char '\VT'
                        "FF"        -> return $ Char '\FF'
                        "CR"        -> return $ Char '\CR'
                        "SO"        -> return $ Char '\SO'
                        "SI"        -> return $ Char '\SI'
                        "DLE"       -> return $ Char '\DLE'
                        "DC1"       -> return $ Char '\DC1'
                        "DC2"       -> return $ Char '\DC2'
                        "DC3"       -> return $ Char '\DC3'
                        "DC4"       -> return $ Char '\DC4'
                        "NAK"       -> return $ Char '\NAK'
                        "SYN"       -> return $ Char '\SYN'
                        "ETB"       -> return $ Char '\ETB'
                        "CAN"       -> return $ Char '\CAN'
                        "EM"        -> return $ Char '\EM'
                        "SUB"       -> return $ Char '\SUB'
                        "ESC"       -> return $ Char '\ESC'
                        "FS"        -> return $ Char '\FS'
                        "GS"        -> return $ Char '\GS'
                        "RS"        -> return $ Char '\RS'
                        "US"        -> return $ Char '\US'
                        "DEL"       -> return $ Char '\DEL'

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
