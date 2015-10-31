module LispInterpreter.LispParser
( parseAtom
, parseFloat
, parseNumber
, parseString
, parseBool
, parseChar
, parseQuoted
, parseList
, parseDottedList
, parseExpr
) where

import Control.Monad
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import LispInterpreter.LispVal

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               return $ Atom (first:rest)

parseFloat :: Parser LispVal
parseFloat = try $ do x <- many1 digit
                      dot <- char '.'
                      y <- many1 digit
                      return $ Float (fst $ readFloat (x ++ [dot] ++ y) !! 0)

parseNumber :: Parser LispVal
parseNumber = do prefix <- (try $ char '#' >> oneOf "box") <|> digit
                 case prefix of
                   'b' -> do numberStr <- many1 $ oneOf "01"
                             return $ Number (bin2dig numberStr)
                   'o' -> do numberStr <- many1 $ oneOf "01234567"
                             return $ Number (fst $ (readOct numberStr) !! 0)
                   'x' -> do numberStr <- many1 $ oneOf "0123456789abcde"
                             return $ Number (fst $ (readHex numberStr) !! 0)
                   _   -> do numberStr <- many digit
                             return $ Number (read $ prefix:numberStr)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedChars <|> noneOf "\"")
                 char '"'
                 return $ String x

parseBool :: Parser LispVal
parseBool = do x <- try $ char '#' >> oneOf "tf"
               case x of
                 't' -> return $ Bool True
                 'f' -> return $ Bool False

parseChar :: Parser LispVal
parseChar = do
    try $ string "#\\"
    value <- spetialChars
             <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
    return $ Character $ case value of
                      "altmode"   -> '\ESC'
                      "backnext"  -> '\US'
                      "backspace" -> '\BS'
                      "call"      -> '\SUB'
                      "linefeed"  -> '\LF'
                      "page"      -> '\FF'
                      "return"    -> '\CR'
                      "rubout"    -> '\DEL'
                      "space"     -> ' '
                      "newline"   -> '\n'
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

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseList :: Parser LispVal
parseList = liftM List $ try $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseFloat
        <|> parseNumber
        <|> parseChar
        <|> parseBool
        <|> parseQuoted
        <|> do char '('
               x <- parseList <|> parseDottedList
               char ')'
               return x


-- Util
--------------------------------------------------------------------------------
bin2dig :: String -> Integer
bin2dig input = foldl (\x y -> x * 2 + y) 0 $ map (\x -> read [x]) input

escapedChars :: Parser Char
escapedChars = do x <- char '\\' >> (oneOf "nrt\\\"")
                  return $ case x of
                             'n' -> '\n'
                             'r' -> '\r'
                             't' -> '\t'
                             _   -> x

spaces :: Parser ()
spaces = skipMany1 space

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
               try (string "newline")   <|>
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

symbol :: Parser Char
symbol = oneOf  "!$%&|*+-/:<=>?@^_~"
