module Scheme.Evaluator.ListFunction
( car
, cdr
, cons
) where

import qualified Control.Monad.Error as Error
import qualified Scheme.LispError as LispError
import qualified Scheme.LispVal as LispVal
import           Scheme.LispVal (LispVal)

car :: [LispVal] -> LispError.ThrowsError LispVal
car [LispVal.List (x : xs)] = return x
car [LispVal.DottedList (x : xs) _] = return x
car [badArg] = Error.throwError $ LispError.TypeMismatch "pair" badArg
car badArgList = Error.throwError $ LispError.NumArgs 1 badArgList

cdr :: [LispVal] -> LispError.ThrowsError LispVal
cdr [LispVal.List (x : xs)] = return $ LispVal.List xs
cdr [LispVal.DottedList [xs] x] = return x
cdr [LispVal.DottedList (x : xs) tail] = return $ LispVal.DottedList xs tail
cdr [badArg] = Error.throwError $ LispError.TypeMismatch "pair" badArg
cdr badArgList = Error.throwError $ LispError.NumArgs 1 badArgList

cons :: [LispVal] -> LispError.ThrowsError LispVal
cons [x, LispVal.List xs] = return $ LispVal.List (x : xs)
cons [x, LispVal.DottedList xs xlast] = return $ LispVal.DottedList (x : xs) xlast
cons [x1, x2] = return $ LispVal.DottedList [x1] x2
cons badArgList = Error.throwError $ LispError.NumArgs 2 badArgList

