import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import Control.Monad
import Data.Char
import Numeric
import Control.Monad.Error

import LispError
import LispVal

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
         `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                       [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Float _) = return val
eval val@(Number _) = return val
eval val@(Character _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         Bool False -> eval alt
         otherwize -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                               then throwError $ TypeMismatch "number" $ String n
                               else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (x : xs) tail] = return $ DottedList xs tail
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                    (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                              Left err -> False
                              Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
