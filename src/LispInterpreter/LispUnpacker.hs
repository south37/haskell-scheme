module LispInterpreter.LispUnpacker
( Unpacker ( AnyUnpacker )
, unpackEquals
) where

import Control.Monad.Error
import LispInterpreter.LispVal
import LispInterpreter.LispError

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
         `catchError` (const $ return False)

