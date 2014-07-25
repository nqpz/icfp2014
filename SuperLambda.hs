-- hybrid between low-level and high-level

import qualified LambdaInstrs as LI

newtype Name = Name String
newtype Arg = Arg String

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
          | Var String -- a variable
          | Cons Expr Expr -- create tuple
          | Car Expr -- get from tuple
          | Cdr Expr -- get second from tuple
          | FunName Name -- function pointer
          | Popped -- take from data stack

data Instr = LIInstr LI.Instr -- base instruction
           | IfThenElse Expr [Instr] [Instr] -- conditional
           | Let Arg Expr -- make a variable and set its value
           | FunDef Name [Arg] [Instr] -- a function
           | CallFun Name [Arg] -- call a function
           | ArraySet Arg Int Expr -- set array value
           | Return Expr -- return value
