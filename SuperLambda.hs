-- hybrid between low-level and high-level

import qualified LambdaMan as LM

newtype Name = Name String

data BinOp = Add
           | Sub
           | Mul
           | Div

data Expr = IntVal LM.Number
          | ArrayVal Int -- length of array
          | Index Int Expr -- take from array
          | BinOp BinOp Expr Expr
          | Arg String -- name of variable
          | Cons Expr Expr
          | Car Expr
          | Cdr Expr
          | FunName Name -- function pointer
          | Popped -- take from data stack

data Instr = LMInstr LM.Instr -- base instruction
           | IfThenElseCall Expr [Instr] [Instr]
           | Let Arg Expr -- make a variable and set its value
           | FunDef Name [Arg] [Instr] -- a function
           | CallFun Name [Arg] -- call a function
           | ArraySet Arg Int Expr -- set array value
