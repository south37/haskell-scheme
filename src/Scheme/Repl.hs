module Scheme.Repl ( runRepl ) where

import qualified System.IO as IO
import qualified Scheme.Interpreter as Interpreter

flushStr :: String -> IO ()
flushStr str = IO.putStr str >> IO.hFlush IO.stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") Interpreter.evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

