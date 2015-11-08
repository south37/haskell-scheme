module Scheme.LispVal
( LispVal
  ( Atom
  , List
  , DottedList
  , Float
  , Number
  , String
  , Bool
  , Character
  )
, unwordsList
, showVal
) where

import qualified Scheme.LispError as LispError

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Float Double
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | PrimitiveFunc ([LispVal] -> LispError.ThrowsError LispVal)
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

