module SuperLambda where

import qualified LambdaInstrs as LI

type Name = String

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Eq
           | Gt
           | Gte

data Expr = IntVal LI.Number -- new int
          | ArrayVal Int -- new array with a length
          | Index Int Expr -- take from array
          | BinOp BinOp Expr Expr -- binary op on ints
          | Var Name -- a variable
          | Cons Expr Expr -- create tuple
          | Car Expr -- get from tuple
          | Cdr Expr -- get second from tuple
          | FunName Name -- function pointer
          | Popped -- take from data stack
          | IfThenElse Expr Expr Expr -- if x then y else z
          | Let Name Expr Expr -- let x = y in z
          | CallFun Name [Var] -- f(a, b, ...)
          | ArraySet Arg Int Expr Expr -- arg[i] = x in y
          | LowLevel LI.Instr Expr -- <low level instruction> in x

data Fun = FunDef Name [Var] Expr

type Super = [Fun]
