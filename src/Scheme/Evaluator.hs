module Scheme.Evaluator ( eval ) where

import qualified Control.Monad.Error as Error
import qualified Scheme.Env as Env
import           Scheme.Env (Env)
import qualified Scheme.IOThrowsError as IOThrowsError
import           Scheme.IOThrowsError (IOThrowsError)
import qualified Scheme.LispError as LispError
import qualified Scheme.LispVal as LispVal
import           Scheme.LispVal (LispVal)
import qualified Scheme.Evaluator.Primitives as Primitives

apply :: String -> [LispVal] -> LispError.ThrowsError LispVal
apply func args = maybe (Error.throwError $ LispError.NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func Primitives.primitives)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(LispVal.String _) = return val
eval env val@(LispVal.Float _) = return val
eval env val@(LispVal.Number _) = return val
eval env val@(LispVal.Character _) = return val
eval env val@(LispVal.Bool _) = return val
eval env (LispVal.Atom id) = Env.getVar env id
eval env (LispVal.List [LispVal.Atom "quote", val]) = return val
eval env (LispVal.List [LispVal.Atom "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of
         LispVal.Bool False -> eval env alt
         otherwize -> eval env conseq
eval env (LispVal.List [LispVal.Atom "set!", LispVal.Atom var, form]) =
    eval env form >>= Env.setVar env var
eval env (LispVal.List [LispVal.Atom "define", LispVal.Atom var, form]) =
    eval env form >>= Env.defineVar env var
eval env (LispVal.List (LispVal.Atom func : args)) =
    mapM (eval env) args >>= IOThrowsError.liftThrows . apply func
eval env badForm =
    Error.throwError $ LispError.BadSpecialForm "Unrecognized special form" badForm

