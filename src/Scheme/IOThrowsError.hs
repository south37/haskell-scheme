module Scheme.IOThrowsError
( IOThrowsError
, liftThrows
, runIOThrows
) where

import qualified Control.Monad.Error as Error
import qualified Scheme.LispError as LispError
import qualified Scheme.Type as Type
import           Scheme.Type (LispError)

type IOThrowsError = Error.ErrorT LispError IO

liftThrows :: Type.ThrowsError a -> IOThrowsError a
liftThrows (Left err) = Error.throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action =
    Error.runErrorT (LispError.trapError action) >>= return . LispError.extractValue
