import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Data.Char
import Numeric
import Control.Monad.Error

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseError) = "Parse error at " ++ show parseError
instance Show LispError where show = showError

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

bin2dig :: String -> Integer
bin2dig input = foldl (\x y -> x * 2 + y) 0 $ map (\x -> read [x]) input

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Float Double
             | Number Integer
             | String String
             | Bool Bool
             | Character Char

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

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Float contents) = show contents
showVal (Number contents) = show contents
showVal (Character contents) = "#\\" ++ [contents]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Float _) = val
eval val@(Number _) = val
eval val@(Character _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                               then 0
                               else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
