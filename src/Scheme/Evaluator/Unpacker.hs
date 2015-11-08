module Scheme.Evaluator.Unpacker
( Unpacker ( AnyUnpacker )
, unpackBool
, unpackEquals
, unpackNum
, unpackStr
) where

import qualified Control.Monad.Error as Error
import qualified Scheme.Type as Type
import           Scheme.Type (LispVal)
import qualified Scheme.LispError as LispError

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> Type.ThrowsError a)

unpackBool :: LispVal -> Type.ThrowsError Bool
unpackBool (Type.Bool b) = return b
unpackBool notBool = Error.throwError $ Type.TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> Type.ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
         `Error.catchError` (const $ return False)

unpackNum :: LispVal -> Type.ThrowsError Integer
unpackNum (Type.Number n) = return n
unpackNum (Type.String n) =
    let parsed = reads n in
        if null parsed
           then Error.throwError
                $ Type.TypeMismatch "number" $ Type.String n
           else return $ fst $ parsed !! 0
unpackNum (Type.List [n]) = unpackNum n
unpackNum notNum = Error.throwError $ Type.TypeMismatch "number" notNum

unpackStr :: LispVal -> Type.ThrowsError String
unpackStr (Type.String s) = return s
unpackStr (Type.Number s) = return $ show s
unpackStr (Type.Bool s) = return $ show s
unpackStr notStr = Error.throwError $ Type.TypeMismatch "string" notStr

