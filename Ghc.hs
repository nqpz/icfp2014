import Data.Word(Word8)

data Arg = Reg Char | IndReg Char | Const Word8 | Mem Word8
data Instr = MOV Arg
           | INC Arg
           | DEC Arg
           | ADD Arg Arg
           | SUB Arg Arg
           | MUL Arg Arg
           | DIV Arg Arg
           | AND Arg Arg
           | OR  Arg Arg
           | XOR Arg Arg
           | JLT Arg Arg
           | JEQ Arg Arg
           | JGT Arg Arg
           | INT Int
           | HLT

-- Data flow analysis:
-- Init: Mem = [0, 0, ... , 0], a = b = 0.
-- Update:
--    
--  
