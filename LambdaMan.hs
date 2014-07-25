{-# LANGUAGE RankNTypes #-}
import Control.Monad.Reader
import Control.Monad.State
import Safe (atMay)

import LambdaInstrs

-- These are the values on the %s stack, that is the data stack
data SValue = SInt     Number
            | SCons    SValue SValue
            | SClosure Addr EStackIds
type SStack = [SValue]

-- These are the values on the %d stack, that is the control stack
data DValue = DJoin     Addr
            | DRet      Addr EStackIds
            | DStop
type DStack = [DValue]

-- These are the values on the %e "stack" -- the environment
type EValue     = [SValue]
type EStackIds  = [Int]
type EStackVals = [EValue]
type EStack     = (EStackIds, EStackVals)

type MachineState = (SStack, DStack, EStack, Addr)
type InstrMonad a = ReaderT [Instr] (State MachineState) a

push_s :: SValue -> InstrMonad ()
push_d :: DValue -> InstrMonad ()
push_e :: EValue -> InstrMonad ()
push_s v = do (s, d, e, c) <- get
              put (v : s, d, e, c)

push_d v = do (s, d, e, c) <- get
              put (s, v : d, e, c)

push_e v = do (s, d, (ei, ev), c) <- get
              put (s, d, (length ev : ei, v : ev), c)

pop_s :: InstrMonad SValue
pop_d :: InstrMonad DValue
pop_s = do (s, d, e, c) <- get
           case s of
             v : s' -> put (s', d, e, c) >> return v
             []     -> fail "Empty %s"

pop_d = do (s, d, e, c) <- get
           case d of
            v : d' -> put (s, d', e, c) >> return v
            []     -> fail "Empty %d"

pop_args :: ArgCount -> InstrMonad EValue
pop_args 0 = return []
pop_args n = do vs <- pop_args (n-1)
                v <- pop_s
                return (v : vs)

get_s :: InstrMonad SStack
get_d :: InstrMonad DStack
get_s = do (s, _, _, _) <- get
           return s

get_d = do (_, d, _, _) <- get
           return d

get_e :: InstrMonad EStack
get_e = do (_, _, e, _) <- get
           return e

put_e :: EStack -> InstrMonad ()
put_e e = do (s, d, _, c) <- get
             put (s, d, e, c)

binop :: (Number -> Number -> Number) -> InstrMonad ()
binop op = do v1 <- pop_s
              v2 <- pop_s
              case (v1, v2) of
                (SInt n1, SInt n2) -> push_s (SInt (op n1 n2))
                _                  -> fail "binop, but top of stack was not ints"

get_c :: InstrMonad Addr
get_c = do (_, _, _, c) <- get
           return c

put_c :: Addr -> InstrMonad ()
put_c c = do (s, d, e, _) <- get
             put (s, d, e, c)

get_instr_at :: Addr -> InstrMonad Instr
get_instr_at a = do mem <- ask
                    case atMay mem (fromIntegral a) of
                      Just i  -> return i
                      Nothing -> fail "Memory out of bounds"

fetch_instr :: InstrMonad Instr
fetch_instr = do c <- get_c
                 put_c (c + 1)
                 get_instr_at c

stepInstr :: InstrMonad ()
stepInstr = stepInstr' =<< fetch_instr

stepInstr' :: Instr -> InstrMonad ()
stepInstr' (LDC n) = push_s (SInt n)
stepInstr' ADD     = binop (+)
stepInstr' SUB     = binop (-)
stepInstr' MUL     = binop (*)
stepInstr' DIV     = binop div
stepInstr' CEQ     = binop (\x y -> if x == y then 1 else 0)
stepInstr' CGT     = binop (\x y -> if x > y  then 1 else 0)
stepInstr' CGTE    = binop (\x y -> if x >= y then 1 else 0)

stepInstr' ATOM =
  do v <- pop_s
     case v of
       SInt _ -> push_s (SInt 1)
       _      -> push_s (SInt 0)

stepInstr' CONS =
  do v1 <- pop_s
     v2 <- pop_s
     push_s (SCons v1 v2)

stepInstr' CAR =
  do v <- pop_s
     case v of
       SCons v1 _ -> push_s v1
       _          -> fail "Expected cons in CAR operator"

stepInstr' CDR =
  do v <- pop_s
     case v of
       SCons _ v2 -> push_s v2
       _          -> fail "Expected cons in CDR operator"

stepInstr' (SEL t f) =
  do v <- pop_s
     case v of
       SInt n -> do c <- get_c
                    push_d (DJoin c)
                    if n == 0 then put_c f else put_c t
       _      -> fail "Expected int in SEL"

stepInstr' JOIN =
  do v <- pop_d
     case v of
       DJoin c -> put_c c
       _       -> fail "Expected join in SEL"

stepInstr' (LDF a) =
  do (ei, _) <- get_e
     push_s (SClosure a ei)

stepInstr' (AP n) =
  do v <- pop_s
     case v of
       SClosure f ei' -> do
         (ei, ev) <- get_e
         ev' <- pop_args n
         c <- get_c

         put_e (length ev : ei', ev ++ [ev'])
         push_d (DRet c ei)
       _ -> fail "AP expects closure"

stepInstr' _ = fail "Unimplemented instruction"
