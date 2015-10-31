import qualified Text.ParserCombinators.Parsec as Parsec
import qualified System.Environment as Environment
import qualified Control.Monad as Monad
import qualified Control.Monad.Error as Error

import LispInterpreter.LispVal(LispVal)
import qualified LispInterpreter.Parser as Parser
import qualified LispInterpreter.Evaluator as Evaluator
import qualified LispInterpreter.LispError as LispError

extractValue :: LispError.ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> LispError.ThrowsError LispVal
readExpr input = case Parsec.parse Parser.parseExpr "lisp" input of
    Left err -> Error.throwError $ LispError.Parser err
    Right val -> return val

main :: IO ()
main = do
    args <- Environment.getArgs
    evaled <- return $ Monad.liftM show $ readExpr (args !! 0) >>= Evaluator.eval
    putStrLn $ extractValue $ LispError.trapError evaled
