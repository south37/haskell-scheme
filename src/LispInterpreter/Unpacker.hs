module LispInterpreter.Unpacker
( Unpacker ( AnyUnpacker )
, unpackEquals
) where

import qualified Control.Monad.Error as Error
import LispInterpreter.LispVal(LispVal)
import qualified LispInterpreter.LispError as LispError

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> LispError.ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> LispError.ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
         `Error.catchError` (const $ return False)

