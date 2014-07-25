module LambdaInstrs where

import Data.Int

type Number    = Int32
type Addr      = Int
type EnvNum    = Int
type EnvOffset = Int
type EnvRef    = (EnvNum, EnvOffset)
type ArgCount  = Int


data Instr = LDC Number      -- load constant
           | LD EnvRef       -- load environment
           | ADD
           | SUB
           | MUL
           | DIV
           | CEQ             -- compare equal
           | CGT             -- compare greater
           | CGTE            -- compare greater equal
           | ATOM            -- is top a number?
           | CONS
           | CAR
           | CDR
           | SEL Addr Addr   -- conditional branch
           | JOIN            -- return from conditional branch
           | LDF Addr        -- load function
           | AP ArgCount     -- call function
           | RTN
           | DUM
           | RAP ArgCount
           | STOP

             -- Tail call extensions
           | TSEL Addr Addr
           | TAP ArgCount
           | TRAP ArgCount

             -- Pascal extensions
           | ST EnvRef

             -- Debug extension
           | DBUG
           | BRK
