module LispInterpreter.Evaluator.Binop
( numericBinop
, boolBinop
, boolBoolBinop
, numBoolBinop
, strBoolBinop
) where

import qualified Control.Monad.Error as Error
import qualified LispInterpreter.LispError as LispError
import qualified LispInterpreter.LispVal as LispVal
import LispInterpreter.LispVal(LispVal)
import qualified LispInterpreter.Evaluator.Unpacker as Unpacker

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispError.ThrowsError LispVal
numericBinop op singleVal@[_] = Error.throwError $ LispError.NumArgs 2 singleVal
numericBinop op params = mapM Unpacker.unpackNum params >>= return . LispVal.Number . foldl1 op


-- BoolBinop
--------------------------------------------------------------------------------
boolBinop :: (LispVal -> LispError.ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> LispError.ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then Error.throwError $ LispError.NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ LispVal.Bool $ left `op` right
numBoolBinop = boolBinop Unpacker.unpackNum
boolBoolBinop = boolBinop Unpacker.unpackBool
strBoolBinop = boolBinop Unpacker.unpackStr

