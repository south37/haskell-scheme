module Scheme.Type
( Env
, LispError
  ( NumArgs
  , TypeMismatch
  , Parser
  , BadSpecialForm
  , NotFunction
  , UnboundVar
  , Default
  )
, LispVal
  ( Atom
  , List
  , DottedList
  , Float
  , Number
  , String
  , Bool
  , Character
  , PrimitiveFunc
  , Func
  )
, showVal
, ThrowsError
) where

import qualified Control.Monad.Error as Error
import qualified Data.IORef as IORef
import qualified Text.ParserCombinators.Parsec as Parsec

type Env = IORef.IORef [(String, IORef.IORef LispVal)]

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
 ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseError) = "Parse error at " ++ show parseError

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Float Double
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String),
                     body :: [LispVal], closure :: Env}

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Float contents) = show contents
showVal (Number contents) = show contents
showVal (Character contents) = "#\\" ++ [contents]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
    "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

type ThrowsError = Either LispError

