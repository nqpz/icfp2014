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
           | Cons
    deriving (Show, Eq)

data UnOp = Car
          | Cdr
          | Atom
    deriving (Show, Eq)

data Expr = IntVal LI.Number          -- [0-9]+
          | Var Name                  -- [a-zA-Z][a-zA-Z0-9_]*
          | BinOp BinOp Expr Expr     -- e1 <binop> e2   OR   (e1, e2)
          | UnOp UnOp Expr            -- {fst,snd,atom} e
          | IfThenElse Expr Expr Expr -- if x then y else z
          | Let [(Name, Expr)] Expr   -- let x = y
                                      -- let z = p
                                      -- in x + z
          | CallFun Expr [Expr]       -- e0 (e1, e2, e3)
          | Lambda [Name] Expr        -- \x y z -> e
          | Seq Stmt Expr
    deriving (Show, Eq)

data Stmt = Print Expr | Break | Store Name Expr
          deriving (Show, Eq)

type Program = Expr
data Fun = FunDef Name [Name] Expr deriving (Eq, Show)
