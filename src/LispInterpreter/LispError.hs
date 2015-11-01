module LispInterpreter.LispError
( LispError
  ( NumArgs
  , TypeMismatch
  , Parser
  , BadSpecialForm
  , NotFunction
  , UnboundVar
  , Default
  )
, showError
, ThrowsError
, trapError
) where

import qualified Control.Monad.Error as Error
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified LispInterpreter.LispVal as LispVal
import           LispInterpreter.LispVal (LispVal)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser Parsec.ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError
instance Error.Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
    "Expected " ++ show expected
 ++ " args; found values " ++ LispVal.unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseError) = "Parse error at " ++ show parseError

type ThrowsError = Either LispError

trapError action = Error.catchError action (return . show)

