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

data Expr = IntVal LI.Number          -- [0-9]+
          | BinOp BinOp Expr Expr     -- e1 <binop> e2
          | Var Name                  -- [a-zA-Z][a-zA-Z0-9_]*
          | Cons Expr Expr            -- (a, b)
          | Car Expr                  -- fst e
          | Cdr Expr                  -- snd e
          | IfThenElse Expr Expr Expr -- if x then y else z
          | Let Name Expr Expr        -- let x = y in z
          | CallFun Expr [Expr]       -- e0 (e1, e2, e3)
          | Lambda [Var] Expr         -- \x y z -> e
