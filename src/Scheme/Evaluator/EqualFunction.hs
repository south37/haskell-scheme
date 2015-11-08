module Scheme.Evaluator.EqualFunction
( eqv
, equal
) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Error as Error
import qualified Scheme.LispError as LispError
import qualified Scheme.LispVal as LispVal
import           Scheme.LispVal (LispVal)
import qualified Scheme.Evaluator.Unpacker as Unpacker

eqv :: [LispVal] -> LispError.ThrowsError LispVal
eqv [left, right] =
    case [left, right] of
         [(LispVal.Bool arg1), (LispVal.Bool arg2)] ->
             return $ LispVal.Bool $ arg1 == arg2
         [(LispVal.Number arg1), (LispVal.Number arg2)] ->
             return $ LispVal.Bool $ arg1 == arg2
         [(LispVal.String arg1), (LispVal.String arg2)] ->
             return $ LispVal.Bool $ arg1 == arg2
         [(LispVal.Atom arg1), (LispVal.Atom arg2)] ->
             return $ LispVal.Bool $ arg1 == arg2
         [(LispVal.DottedList xs x), (LispVal.DottedList ys y)] ->
             eqv [LispVal.List $ xs ++ [x], LispVal.List $ ys ++ [y]]
         [(LispVal.List arg1), (LispVal.List arg2)] ->
             return $ LispVal.Bool $ (length arg1 == length arg2) &&
                    (all eqvPair $ zip arg1 arg2)
                where eqvPair (x1, x2) = case eqv [x1, x2] of
                                              Left err -> False
                                              Right (LispVal.Bool val) -> val
         [_, _] -> return $ LispVal.Bool False
eqv badArgList = Error.throwError $ LispError.NumArgs 2 badArgList

equal :: [LispVal] -> LispError.ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- Monad.liftM or $ mapM (Unpacker.unpackEquals arg1 arg2)
                       [ Unpacker.AnyUnpacker Unpacker.unpackNum
                       , Unpacker.AnyUnpacker Unpacker.unpackStr
                       , Unpacker.AnyUnpacker Unpacker.unpackBool ]
    eqvEquals <- eqv [arg1, arg2]
    return $ LispVal.Bool $ (primitiveEquals || let (LispVal.Bool x) = eqvEquals in x)

equal badArgList = Error.throwError $ LispError.NumArgs 2 badArgList
