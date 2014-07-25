module SuperComp where

import LambdaInstrs
import SuperLambda

import Control.Monad.State
import Control.Monad.Writer
import Data.List

type Frame     = [Name]
type Frames    = [Frame]
type VarCount  = Int
type CompState = (Frames, VarCount)
type CompMonad a = WriterT ([Instr], [Instr]) (State CompState) a

get_frames :: CompMonad Frames
get_frames = fmap fst get

put_frames :: Frames -> CompMonad ()
put_frames f= do (_, n) <- get
                 put (f, n)

fresh_label :: String -> CompMonad String
fresh_label s = do (f, n) <- get
                   put (f, n + 1)
                   return (s ++ '_' : show n)

tellCur, tellLater :: Instr -> CompMonad ()
tellCur   i = tell ([i], [])
tellLater i = tell ([], [i])

compBinop :: BinOp -> CompMonad ()
compBinop Add  = tellCur ADD
compBinop Sub  = tellCur SUB
compBinop Mul  = tellCur MUL
compBinop Div  = tellCur DIV
compBinop Eq   = tellCur CEQ
compBinop Gt   = tellCur CGT
compBinop Gte  = tellCur CGTE
compBinop Cons = tellCur CONS

compUnop :: UnOp -> CompMonad ()
compUnop Car  = tellCur CAR
compUnop Cdr  = tellCur CDR
compUnop Atom = tellCur ATOM

findVar :: Name -> CompMonad (Int, Int)
findVar s = findVar' 0 =<< get_frames
  where findVar' _ []        = error ("Could not find variable " ++ s)
        findVar' n (f : fs) =
          case elemIndex s f of
            Just n' -> return (n, n')
            Nothing -> findVar' (n+1) fs

compExpr :: Expr -> CompMonad ()
compExpr (IntVal n)       = tellCur (LDC n)
compExpr (BinOp op e1 e2) = compExpr e2 >> compExpr e1 >> compBinop op
compExpr (UnOp op e)      = compExpr e  >> compUnop op
compExpr (Var n)    = do
  (enum, eoff) <- findVar n
  tellCur (LD (enum, eoff))
compExpr (IfThenElse e0 eT eF) = do
  lT <- fresh_label "true"
  lF <- fresh_label "false"
  lA <- fresh_label "after"
  compExpr e0
  tellCur (TSEL (Lab lT) (Lab lF))
  tellCur (LABEL lT)
  compExpr eT
  tellCur (LDC 0)
  tellCur (TSEL (Lab lA) (Lab lA))
  tellCur (LABEL lF)
  compExpr eF
  tellCur (LABEL lA)
compExpr (Let binds e2) = do
  let (names, exprs) = unzip binds

  tellCur (DUM (length names))
  fs <- get_frames
  put_frames (names : fs)

  label <- fresh_label "let"
  mapM compExpr exprs

  tellCur (LDF (Lab label))
  tellCur (TRAP (length names))
  tellCur (LABEL label)
compExpr (CallFun eF eArgs) = do
  mapM compExpr eArgs
  compExpr eF
  tellCur (AP (length eArgs))
compExpr (Lambda ns e) = do
  label <- fresh_label "lambda"
  fs <- get_frames

  pass $ do
    put_frames (ns : fs)
    tellCur (LABEL label)
    compExpr e
    return ((), \(cur, later) -> ([], cur ++ later))

  put_frames fs

runComp :: CompMonad () -> [Instr]
runComp m =
  let s  = execWriterT m :: State CompState ([Instr], [Instr])
      (cur, later) = evalState s ([], 0)
  in cur ++ later

runExpr :: Expr -> [Instr]
runExpr  = runComp . compExpr
