module Scheme.Evaluator ( eval ) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Error as Error
import qualified Scheme.Env as Env
import qualified Scheme.IOThrowsError as IOThrowsError
import           Scheme.IOThrowsError (IOThrowsError)
import qualified Scheme.LispError as LispError
import qualified Scheme.Evaluator.Primitives as Primitives
import qualified Scheme.Type as Type
import           Scheme.Type (Env, LispVal)

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (Type.PrimitiveFunc func) args = IOThrowsError.liftThrows $ func args
apply (Type.Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
      then Error.throwError $ Type.NumArgs  (num params) args
      else (Error.liftIO $ Env.bindVars closure $ zip params args) >>=
           bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = Monad.liftM last $ Monad.mapM (eval env) body
          bindVarArgs arg env = case arg of
              Just argName -> Error.liftIO $ Env.bindVars env [(argName, Type.List $ remainingArgs)]
              Nothing -> return env

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(Type.String _) = return val
eval env val@(Type.Float _) = return val
eval env val@(Type.Number _) = return val
eval env val@(Type.Character _) = return val
eval env val@(Type.Bool _) = return val
eval env (Type.Atom id) = Env.getVar env id
eval env (Type.List [Type.Atom "quote", val]) = return val
eval env (Type.List [Type.Atom "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of
         Type.Bool False -> eval env alt
         otherwize -> eval env conseq
eval env (Type.List [Type.Atom "set!", Type.Atom var, form]) =
    eval env form >>= Env.setVar env var
eval env (Type.List [Type.Atom "define", Type.Atom var, form]) =
    eval env form >>= Env.defineVar env var
eval env (Type.List (Type.Atom "define" : Type.List (Type.Atom var : params) : body)) =
    makeNormalFunc env params body >>= Env.defineVar env var
eval env (Type.List (Type.Atom "define" : Type.DottedList (Type.Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= Env.defineVar env var
eval env (Type.List (Type.Atom "lambda" : Type.List params : body)) =
    makeNormalFunc env params body
eval env (Type.List (Type.Atom "lambda" : Type.DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (Type.List (Type.Atom "lambda" : varargs@(Type.Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (Type.List (function : args)) = do
    func <- eval env function
    argVals <- Monad.mapM (eval env) args
    apply func argVals
eval env badForm =
    Error.throwError $ Type.BadSpecialForm "Unrecognized special form" badForm

makeFunc varargs env params body = return $ Type.Func (map Type.showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . Type.showVal
