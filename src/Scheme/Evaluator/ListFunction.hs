module Scheme.Evaluator.ListFunction
( car
, cdr
, cons
) where

import qualified Control.Monad.Error as Error
import qualified Scheme.Type as Type
import           Scheme.Type (LispVal)

car :: [LispVal] -> Type.ThrowsError LispVal
car [Type.List (x : xs)] = return x
car [Type.DottedList (x : xs) _] = return x
car [badArg] = Error.throwError $ Type.TypeMismatch "pair" badArg
car badArgList = Error.throwError $ Type.NumArgs 1 badArgList

cdr :: [LispVal] -> Type.ThrowsError LispVal
cdr [Type.List (x : xs)] = return $ Type.List xs
cdr [Type.DottedList [xs] x] = return x
cdr [Type.DottedList (x : xs) tail] = return $ Type.DottedList xs tail
cdr [badArg] = Error.throwError $ Type.TypeMismatch "pair" badArg
cdr badArgList = Error.throwError $ Type.NumArgs 1 badArgList

cons :: [LispVal] -> Type.ThrowsError LispVal
cons [x, Type.List xs] = return $ Type.List (x : xs)
cons [x, Type.DottedList xs xlast] = return $ Type.DottedList (x : xs) xlast
cons [x1, x2] = return $ Type.DottedList [x1] x2
cons badArgList = Error.throwError $ Type.NumArgs 2 badArgList

