module LispInterpreter.Evaluator.Unpacker
( Unpacker ( AnyUnpacker )
, unpackBool
, unpackEquals
, unpackNum
, unpackStr
) where

import qualified Control.Monad.Error as Error
import qualified LispInterpreter.LispVal as LispVal
import LispInterpreter.LispVal(LispVal)
import qualified LispInterpreter.LispError as LispError

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> LispError.ThrowsError a)

unpackBool :: LispVal -> LispError.ThrowsError Bool
unpackBool (LispVal.Bool b) = return b
unpackBool notBool = Error.throwError $ LispError.TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> LispError.ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
         `Error.catchError` (const $ return False)

unpackNum :: LispVal -> LispError.ThrowsError Integer
unpackNum (LispVal.Number n) = return n
unpackNum (LispVal.String n) =
    let parsed = reads n in
        if null parsed
           then Error.throwError
                $ LispError.TypeMismatch "number" $ LispVal.String n
           else return $ fst $ parsed !! 0
unpackNum (LispVal.List [n]) = unpackNum n
unpackNum notNum = Error.throwError $ LispError.TypeMismatch "number" notNum

unpackStr :: LispVal -> LispError.ThrowsError String
unpackStr (LispVal.String s) = return s
unpackStr (LispVal.Number s) = return $ show s
unpackStr (LispVal.Bool s) = return $ show s
unpackStr notStr = Error.throwError $ LispError.TypeMismatch "string" notStr

