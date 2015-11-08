import qualified System.Environment as Environment
import qualified Scheme.Interpreter as Interpreter

main :: IO ()
main = do args <- Environment.getArgs
          case length args of
            0 ->         Interpreter.runRepl
            1 ->         Interpreter.evalAndPrint $ args !! 0
            otherwise -> putStrLn "Program takes only 0 or 1 argument"
