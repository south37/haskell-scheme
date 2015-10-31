import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import Control.Monad
import Data.Char
import Control.Monad.Error

import LispVal
import LispParser
import LispEvaluator
import LispError

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
