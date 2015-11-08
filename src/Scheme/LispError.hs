module Scheme.LispError
( extractValue
, trapError
) where

import qualified Control.Monad.Error as Error
import           Scheme.Type (ThrowsError)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

trapError action = Error.catchError action (return . show)

