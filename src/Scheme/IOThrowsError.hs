module Scheme.IOThrowsError
( IOThrowsError
, liftThrows
, runIOThrows
) where

import qualified Control.Monad.Error as Error
import qualified Scheme.LispError as LispError
import           Scheme.Type (LispError, ThrowsError)

type IOThrowsError = Error.ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = Error.throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action =
    Error.runErrorT (LispError.trapError action) >>= return . LispError.extractValue
