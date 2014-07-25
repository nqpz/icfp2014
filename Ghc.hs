data Arg = Const Int | Reg String | Mem String
data Instr = Mov Reg Reg
           | INC Reg
           | DEC Reg
           | ADD Reg Reg
           | SUB Reg Reg
           | MUL Reg Reg
           | DIV Reg Reg
           | AND Reg Reg
           | OR  Reg Reg
           | XOR Reg Reg
           | JLT Reg Reg
           | JEQ Reg Reg
           | JGT Reg Reg
           | INT Int
           | HLT


-- Data flow analysis:
-- Init: Mem = [0, 0, ... , 0], a = b = 0.
-- Update:
--    
--  
