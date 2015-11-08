module Scheme.Evaluator.EqualFunction
( eqv
, equal
) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Error as Error
import qualified Scheme.Type as Type
import           Scheme.Type (LispVal)
import qualified Scheme.Evaluator.Unpacker as Unpacker

eqv :: [LispVal] -> Type.ThrowsError LispVal
eqv [left, right] =
    case [left, right] of
         [(Type.Bool arg1), (Type.Bool arg2)] ->
             return $ Type.Bool $ arg1 == arg2
         [(Type.Number arg1), (Type.Number arg2)] ->
             return $ Type.Bool $ arg1 == arg2
         [(Type.String arg1), (Type.String arg2)] ->
             return $ Type.Bool $ arg1 == arg2
         [(Type.Atom arg1), (Type.Atom arg2)] ->
             return $ Type.Bool $ arg1 == arg2
         [(Type.DottedList xs x), (Type.DottedList ys y)] ->
             eqv [Type.List $ xs ++ [x], Type.List $ ys ++ [y]]
         [(Type.List arg1), (Type.List arg2)] ->
             return $ Type.Bool $ (length arg1 == length arg2) &&
                    (all eqvPair $ zip arg1 arg2)
                where eqvPair (x1, x2) = case eqv [x1, x2] of
                                              Left err -> False
                                              Right (Type.Bool val) -> val
         [_, _] -> return $ Type.Bool False
eqv badArgList = Error.throwError $ Type.NumArgs 2 badArgList

equal :: [LispVal] -> Type.ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- Monad.liftM or $ mapM (Unpacker.unpackEquals arg1 arg2)
                       [ Unpacker.AnyUnpacker Unpacker.unpackNum
                       , Unpacker.AnyUnpacker Unpacker.unpackStr
                       , Unpacker.AnyUnpacker Unpacker.unpackBool ]
    eqvEquals <- eqv [arg1, arg2]
    return $ Type.Bool $ (primitiveEquals || let (Type.Bool x) = eqvEquals in x)

equal badArgList = Error.throwError $ Type.NumArgs 2 badArgList

