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
          | BinOp BinOp Expr Expr -- binary op on ints
          | Var Name -- a variable
          | Cons Expr Expr -- (a, b)
          | Car Expr -- let (a, _) in a // fst
          | Cdr Expr -- let (_, b) in b // snd
          | IfThenElse Expr Expr Expr -- if x then y else z
          | Let Name Expr Expr -- let x = y in z
          | CallFun Name [Var] -- f(a, b, ...)
          | LowLevel LI.Instr -- <low level instruction> 

data Fun = FunDef Name [Var] Expr

type Super = [Fun]
