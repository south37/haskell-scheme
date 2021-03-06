module Scheme.Env
( bindVars
, defineVar
, getVar
, nullEnv
, primitiveBindings
, setVar
) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Error as Error
import qualified Data.IORef as IORef
import qualified Scheme.Evaluator.Primitives as Primitives
import           Scheme.IOThrowsError (IOThrowsError)
import qualified Scheme.Type as Type
import           Scheme.Type (Env, LispVal)

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
    IORef.readIORef envRef >>= extendEnv bindings >>= IORef.newIORef
    where extendEnv bindings env = Monad.liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- IORef.newIORef value
                                       return (var, ref)

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value =
    do alreadyDefined <- Error.liftIO $ isBound envRef var
       if alreadyDefined
         then setVar envRef var value >> return value
         else Error.liftIO $ do
             valueRef <- IORef.newIORef value
             env <- IORef.readIORef envRef
             IORef.writeIORef envRef ((var, valueRef) : env)
             return value

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var =
    do env <- Error.liftIO $ IORef.readIORef envRef
       maybe (Error.throwError $ Type.UnboundVar "Getting an unbound variable: " var)
             (Error.liftIO . IORef.readIORef)
             (lookup var env)

isBound :: Env -> String -> IO Bool
isBound envRef var =
    do env <- IORef.readIORef envRef
       return . maybe False (const True) $ lookup var env

nullEnv :: IO Env
nullEnv = IORef.newIORef []

primitiveBindings :: IO Env
primitiveBindings =
    nullEnv >>= (flip bindVars $ map makePrimitiveFunc Primitives.primitives)
    where makePrimitiveFunc (var, func) = (var, Type.PrimitiveFunc func)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value =
    do env <- Error.liftIO $ IORef.readIORef envRef
       maybe (Error.throwError $ Type.UnboundVar "Setting an unbound variable: " var)
             (Error.liftIO . (flip IORef.writeIORef value))
             (lookup var env)
       return value

