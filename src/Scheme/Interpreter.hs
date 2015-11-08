module Scheme.Interpreter ( evalAndPrint ) where

import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Control.Monad as Monad
import qualified Control.Monad.Error as Error

import           Scheme.LispVal (LispVal)
import qualified Scheme.Parser as Parser
import qualified Scheme.Evaluator as Evaluator
import qualified Scheme.LispError as LispError

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

evalString :: String -> IO String
evalString expr =
    let evaled = readExpr expr >>= Evaluator.eval
    in return $ LispError.extractValue $ LispError.trapError (Monad.liftM show evaled)

readExpr :: String -> LispError.ThrowsError LispVal
readExpr input = case Parsec.parse Parser.parseExpr "lisp" input of
    Left err -> Error.throwError $ LispError.Parser err
    Right val -> return val
