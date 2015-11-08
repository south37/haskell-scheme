module Scheme.Interpreter
( evalAndPrint
, runOne
) where

import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Control.Monad as Monad
import qualified Control.Monad.Error as Error

import qualified Scheme.Env as Env
import           Scheme.Env (Env)
import qualified Scheme.IOThrowsError as IOThrowsError
import           Scheme.LispVal (LispVal)
import qualified Scheme.Parser as Parser
import qualified Scheme.Evaluator as Evaluator
import qualified Scheme.LispError as LispError

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr =
    let evaled = (IOThrowsError.liftThrows $ readExpr expr) >>= Evaluator.eval env
    in IOThrowsError.runIOThrows (Monad.liftM show evaled)

readExpr :: String -> LispError.ThrowsError LispVal
readExpr input = case Parsec.parse Parser.parseExpr "lisp" input of
    Left err -> Error.throwError $ LispError.Parser err
    Right val -> return val

runOne :: String -> IO ()
runOne expr = Env.primitiveBindings >>= flip evalAndPrint expr

