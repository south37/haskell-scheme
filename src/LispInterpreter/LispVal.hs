module LispInterpreter.LispVal
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

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Float Double
             | Number Integer
             | String String
             | Bool Bool
             | Character Char

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
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

