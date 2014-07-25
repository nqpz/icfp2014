import Control.Monad.Reader
import Control.Monad.State
import Safe
import Data.Int

type Number    = Int32
type Addr      = Int
type EnvNum    = Int
type EnvOffset = Int
type EnvRef    = (EnvNum, EnvOffset)
type ArgCount  = Int

-- These are the values on the %s stack, that is the data stack
data SValue = SInt     Number
            | SCons    SValue SValue
            | SClosure Addr EStack
type SStack = [SValue]

-- These are the values on the %d stack, that is the control stack
data DValue = DJoin     Addr
            | DRet      Addr EStack
            | DStop
type DStack = [DValue]

-- These are the values on the %e "stack" -- the environment
type EValue = [SValue]
type EStack = [EValue]


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

             -- Extensions
           | TSEL Addr Addr
           | TAP Addr Addr

type MachineState = (SStack, DStack, EStack, Addr)
type InstrMonad a = ReaderT [Instr]  (State MachineState) a

push_s :: SValue -> InstrMonad ()
push_d :: DValue -> InstrMonad ()
push_e :: EValue -> InstrMonad ()
push_s v = do (s, d, e, c) <- get
              put (v : s, d, e, c)

push_d v = do (s, d, e, c) <- get
              put (s, v : d, e, c)

push_e v = do (s, d, e, c) <- get
              put (s, d, v : e, c)


pop_s :: InstrMonad SValue
pop_d :: InstrMonad DValue
pop_e :: InstrMonad EValue
pop_s = do (s, d, e, c) <- get
           case s of
             v : s' -> put (s', d, e, c) >> return v
             []     -> fail "Empty %s"

pop_d = do (s, d, e, c) <- get
           case d of
            v : d' -> put (s, d', e, c) >> return v
            []     -> fail "Empty %d"

pop_e = do (s, d, e, c) <- get
           case e of
             v : e' -> put (s, d, e', c) >> return v
             []     -> fail "Empty %d"

peek_s :: InstrMonad (Maybe SValue)
peek_d :: InstrMonad (Maybe DValue)
peek_e :: InstrMonad (Maybe EValue)
peek_s = do (s, _, _, _) <- get
            case s of
              v : _ -> return (Just v)
              []    -> return Nothing

peek_d = do (_, d, _, _) <- get
            case d of
              v : _ -> return (Just v)
              []    -> return Nothing

peek_e = do (_, _, e, _) <- get
            case e of
              v : _ -> return (Just v)
              []    -> return Nothing

binop :: (Number -> Number -> Number) -> InstrMonad ()
binop op = do v1 <- pop_s
              v2 <- pop_s
              case (v1, v2) of
                (SInt n1, SInt n2) -> push_s (SInt (op n1 n2))
                _                  -> fail "binop, but top of stack was not ints"

get_instr_at :: Addr -> InstrMonad Instr
get_instr_at a = do mem <- ask
                    case atMay mem (fromIntegral a) of
                      Just i  -> return i
                      Nothing -> fail "Memory out of bounds"

get_instr :: InstrMonad Instr
get_instr = do (_, _, _, c) <- get
               get_instr_at c

stepInstr :: InstrMonad ()
stepInstr = stepInstr' =<< get_instr

stepInstr' :: Instr -> InstrMonad ()
stepInstr' (LDC n) = push_s (SInt n)
stepInstr' ADD     = binop (+)
stepInstr' SUB     = binop (-)
stepInstr' MUL     = binop (*)
stepInstr' DIV     = binop div
stepInstr' CEQ     = binop (\x y -> if x == y then 1 else 0)
stepInstr' CGT     = binop (\x y -> if x > y  then 1 else 0)
stepInstr' CGTE    = binop (\x y -> if x >= y then 1 else 0)
stepInstr' ATOM    =
  do v <- pop_s
     case v of
       SInt _ -> push_s (SInt 1)
       _      -> push_s (SInt 0)
stepInstr' CONS    =
  do v1 <- pop_s
     v2 <- pop_s
     push_s (SCons v1 v2)
-- stepInstr' CAR     =
--   do v <- pop_s
--      case v of

--      v2 <- pop_s
--      push_s (SCons v1 v2)
stepInstr' _       = fail "Not implemented"
