module Scheme.Evaluator.Primitives ( primitives ) where

import qualified Scheme.Evaluator.Binop as Binop
import qualified Scheme.Evaluator.EqualFunction as EqualFunction
import qualified Scheme.Evaluator.ListFunction as ListFunction
import           Scheme.Type (LispVal, ThrowsError)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

