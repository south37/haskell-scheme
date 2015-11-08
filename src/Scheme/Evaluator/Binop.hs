module Scheme.Evaluator.Binop
( numericBinop
, boolBinop
, boolBoolBinop
, numBoolBinop
, strBoolBinop
) where

import qualified Control.Monad.Error as Error
import qualified Scheme.LispError as LispError
import qualified Scheme.Evaluator.Unpacker as Unpacker
import qualified Scheme.Type as Type
import           Scheme.Type (LispVal)

numericBinop :: (Integer -> Integer -> Integer)
                -> [LispVal] -> Type.ThrowsError LispVal
numericBinop op singleVal@[_] = Error.throwError $ Type.NumArgs 2 singleVal
numericBinop op params =
    mapM Unpacker.unpackNum params >>= return . Type.Number . foldl1 op


-- BoolBinop
--------------------------------------------------------------------------------
boolBinop :: (LispVal -> Type.ThrowsError a)
             -> (a -> a -> Bool) -> [LispVal] -> Type.ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then Error.throwError $ Type.NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Type.Bool $ left `op` right
numBoolBinop = boolBinop Unpacker.unpackNum
boolBoolBinop = boolBinop Unpacker.unpackBool
strBoolBinop = boolBinop Unpacker.unpackStr

