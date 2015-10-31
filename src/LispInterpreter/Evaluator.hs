module LispInterpreter.Evaluator ( eval ) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Error as Error
import qualified LispInterpreter.Unpacker as Unpacker
import qualified LispInterpreter.LispVal as LispVal
import LispInterpreter.LispVal(LispVal)
import qualified LispInterpreter.LispError as LispError

apply :: String -> [LispVal] -> LispError.ThrowsError LispVal
apply func args = maybe (Error.throwError $ LispError.NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

eval :: LispVal -> LispError.ThrowsError LispVal
eval val@(LispVal.String _) = return val
eval val@(LispVal.Float _) = return val
eval val@(LispVal.Number _) = return val
eval val@(LispVal.Character _) = return val
eval val@(LispVal.Bool _) = return val
eval (LispVal.List [LispVal.Atom "quote", val]) = return val
eval (LispVal.List [LispVal.Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         LispVal.Bool False -> eval alt
         otherwize -> eval conseq
eval (LispVal.List (LispVal.Atom func : args)) = mapM eval args >>= apply func
eval badForm = Error.throwError $ LispError.BadSpecialForm "Unrecognized special form" badForm

primitives :: [(String, [LispVal] -> LispError.ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

boolBinop :: (LispVal -> LispError.ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> LispError.ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then Error.throwError $ LispError.NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ LispVal.Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispError.ThrowsError LispVal
numericBinop op singleVal@[_] = Error.throwError $ LispError.NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . LispVal.Number . foldl1 op

unpackNum :: LispVal -> LispError.ThrowsError Integer
unpackNum (LispVal.Number n) = return n
unpackNum (LispVal.String n) = let parsed = reads n in
                           if null parsed
                               then Error.throwError $ LispError.TypeMismatch "number" $ LispVal.String n
                               else return $ fst $ parsed !! 0
unpackNum (LispVal.List [n]) = unpackNum n
unpackNum notNum = Error.throwError $ LispError.TypeMismatch "number" notNum

unpackBool :: LispVal -> LispError.ThrowsError Bool
unpackBool (LispVal.Bool b) = return b
unpackBool notBool = Error.throwError $ LispError.TypeMismatch "boolean" notBool

unpackStr :: LispVal -> LispError.ThrowsError String
unpackStr (LispVal.String s) = return s
unpackStr (LispVal.Number s) = return $ show s
unpackStr (LispVal.Bool s) = return $ show s
unpackStr notStr = Error.throwError $ LispError.TypeMismatch "string" notStr

car :: [LispVal] -> LispError.ThrowsError LispVal
car [LispVal.List (x : xs)] = return x
car [LispVal.DottedList (x : xs) _] = return x
car [badArg] = Error.throwError $ LispError.TypeMismatch "pair" badArg
car badArgList = Error.throwError $ LispError.NumArgs 1 badArgList

cdr :: [LispVal] -> LispError.ThrowsError LispVal
cdr [LispVal.List (x : xs)] = return $ LispVal.List xs
cdr [LispVal.DottedList [xs] x] = return x
cdr [LispVal.DottedList (x : xs) tail] = return $ LispVal.DottedList xs tail
cdr [badArg] = Error.throwError $ LispError.TypeMismatch "pair" badArg
cdr badArgList = Error.throwError $ LispError.NumArgs 1 badArgList

cons :: [LispVal] -> LispError.ThrowsError LispVal
cons [x, LispVal.List xs] = return $ LispVal.List (x : xs)
cons [x, LispVal.DottedList xs xlast] = return $ LispVal.DottedList (x : xs) xlast
cons [x1, x2] = return $ LispVal.DottedList [x1] x2
cons badArgList = Error.throwError $ LispError.NumArgs 2 badArgList

eqv :: [LispVal] -> LispError.ThrowsError LispVal
eqv [(LispVal.Bool arg1), (LispVal.Bool arg2)] = return $ LispVal.Bool $ arg1 == arg2
eqv [(LispVal.Number arg1), (LispVal.Number arg2)] = return $ LispVal.Bool $ arg1 == arg2
eqv [(LispVal.String arg1), (LispVal.String arg2)] = return $ LispVal.Bool $ arg1 == arg2
eqv [(LispVal.Atom arg1), (LispVal.Atom arg2)] = return $ LispVal.Bool $ arg1 == arg2
eqv [(LispVal.DottedList xs x), (LispVal.DottedList ys y)] = eqv [LispVal.List $ xs ++ [x], LispVal.List $ ys ++ [y]]
eqv [(LispVal.List arg1), (LispVal.List arg2)] = return $ LispVal.Bool $ (length arg1 == length arg2) &&
                                                    (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                              Left err -> False
                              Right (LispVal.Bool val) -> val
eqv [_, _] = return $ LispVal.Bool False
eqv badArgList = Error.throwError $ LispError.NumArgs 2 badArgList

equal :: [LispVal] -> LispError.ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- Monad.liftM or $ mapM (Unpacker.unpackEquals arg1 arg2)
                       [ Unpacker.AnyUnpacker unpackNum
                       , Unpacker.AnyUnpacker unpackStr
                       , Unpacker.AnyUnpacker unpackBool ]
    eqvEquals <- eqv [arg1, arg2]
    return $ LispVal.Bool $ (primitiveEquals || let (LispVal.Bool x) = eqvEquals in x)
equal badArgList = Error.throwError $ LispError.NumArgs 2 badArgList

