module Scheme.LispError
( extractValue
, trapError
) where

import qualified Control.Monad.Error as Error
import qualified Scheme.Type as Type

extractValue :: Type.ThrowsError a -> a
extractValue (Right val) = val

trapError action = Error.catchError action (return . show)

