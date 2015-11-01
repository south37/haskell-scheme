module LispInterpreter.Evaluator ( eval ) where

import qualified Control.Monad.Error as Error
import qualified LispInterpreter.LispError as LispError
import qualified LispInterpreter.LispVal as LispVal
import           LispInterpreter.LispVal (LispVal)
import qualified LispInterpreter.Evaluator.Primitives as Primitives

apply :: String -> [LispVal] -> LispError.ThrowsError LispVal
apply func args = maybe (Error.throwError $ LispError.NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func Primitives.primitives)

eval :: LispVal -> LispError.ThrowsError LispVal
eval val@(LispVal.String _) = return val
eval val@(LispVal.Float _) = return val
eval val@(LispVal.Number _) = return val
eval val@(LispVal.Character _) = return val
eval val@(LispVal.Bool _) = return val
eval (LispVal.List [LispVal.Atom "quote", val]) = return val
eval (LispVal.List [LispVal.Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         LispVal.Bool False -> eval alt
         otherwize -> eval conseq
eval (LispVal.List (LispVal.Atom func : args)) = mapM eval args >>= apply func
eval badForm = Error.throwError $ LispError.BadSpecialForm "Unrecognized special form" badForm

