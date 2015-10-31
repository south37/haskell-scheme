module LispInterpreter.Evaluator.Primitives ( primitives ) where

import qualified LispInterpreter.LispError as LispError
import LispInterpreter.LispVal(LispVal)
import qualified LispInterpreter.Evaluator.Binop as Binop
import qualified LispInterpreter.Evaluator.ListFunction as ListFunction
import qualified LispInterpreter.Evaluator.EqualFunction as EqualFunction

primitives :: [(String, [LispVal] -> LispError.ThrowsError LispVal)]
primitives = [("+", Binop.numericBinop (+)),
              ("-", Binop.numericBinop (-)),
              ("*", Binop.numericBinop (*)),
              ("/", Binop.numericBinop div),
              ("mod", Binop.numericBinop mod),
              ("quotient", Binop.numericBinop quot),
              ("remainder", Binop.numericBinop rem),
              ("=", Binop.numBoolBinop (==)),
              ("<", Binop.numBoolBinop (<)),
              (">", Binop.numBoolBinop (>)),
              ("/=", Binop.numBoolBinop (/=)),
              (">=", Binop.numBoolBinop (>=)),
              ("<=", Binop.numBoolBinop (<=)),
              ("&&", Binop.boolBoolBinop (&&)),
              ("||", Binop.boolBoolBinop (||)),
              ("string=?", Binop.strBoolBinop (==)),
              ("string<?", Binop.strBoolBinop (<)),
              ("string>?", Binop.strBoolBinop (>)),
              ("string<=?", Binop.strBoolBinop (<=)),
              ("string>=?", Binop.strBoolBinop (>=)),
              ("car", ListFunction.car),
              ("cdr", ListFunction.cdr),
              ("cons", ListFunction.cons),
              ("eq?", EqualFunction.eqv),
              ("eqv?", EqualFunction.eqv),
              ("equal?", EqualFunction.equal)]

