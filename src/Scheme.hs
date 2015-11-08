import qualified Text.ParserCombinators.Parsec as Parsec
import qualified System.Environment as Environment
import qualified System.IO as IO
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
    in return $ extractValue $ LispError.trapError (Monad.liftM show evaled)

extractValue :: LispError.ThrowsError a -> a
extractValue (Right val) = val

flushStr :: String -> IO ()
flushStr str = IO.putStr str >> IO.hFlush IO.stdout

readExpr :: String -> LispError.ThrowsError LispVal
readExpr input = case Parsec.parse Parser.parseExpr "lisp" input of
    Left err -> Error.throwError $ LispError.Parser err
    Right val -> return val

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

main :: IO ()
main = do args <- Environment.getArgs
          case length args of
            0 ->         runRepl
            1 ->         evalAndPrint $ args !! 0
            otherwise -> putStrLn "Program takes only 0 or 1 argument"
