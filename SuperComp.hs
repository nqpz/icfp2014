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
compBinop Add   = tellCur ADD
compBinop Sub   = tellCur SUB
compBinop Mul   = tellCur MUL
compBinop Div   = tellCur DIV
compBinop Eq    = tellCur CEQ
compBinop Gt    = tellCur CGT
compBinop Gte   = tellCur CGTE
compBinop Cons  = tellCur CONS

compUnop :: UnOp -> CompMonad ()
compUnop Car   = tellCur CAR
compUnop Cdr   = tellCur CDR
compUnop Atom  = tellCur ATOM

findVar :: Name -> CompMonad (Int, Int)
findVar s = findVar' 0 =<< get_frames
  where findVar' _ []        = error ("Could not find variable " ++ s)
        findVar' n (f : fs) =
          case elemIndex s f of
            Just n' -> return (n, n')
            Nothing -> findVar' (n+1) fs

compStmt :: Stmt -> CompMonad ()
compStmt (Print e)   = compExpr False e >> tellCur DBUG
compStmt Break       = tellCur BRK
compStmt (Store n e) = do
  compExpr False e
  pos <- findVar n
  tellCur (LD pos)

compExpr :: Bool -> Expr -> CompMonad ()
compExpr t (IntVal n)       = tellCur (LDC n)   >> when t (tellCur RTN)
compExpr t (Seq s e)        = compStmt s        >> compExpr t e
compExpr t (BinOp op e1 e2) = compExpr False e1 >> compExpr False e2 >> compBinop op >> when t (tellCur RTN)
compExpr t (UnOp op e)      = compExpr False e  >> compUnop op >> when t (tellCur RTN)
compExpr t (Var n)    = do
  pos <- findVar n
  tellCur (LD pos)
  when t (tellCur RTN)
compExpr t (IfThenElse e0 eT eF) = do
  lT <- fresh_label "true"
  lF <- fresh_label "false"
  lA <- fresh_label "after"
  compExpr False e0
  tellCur (TSEL (Lab lT) (Lab lF))

  tellCur (LABEL lT)
  compExpr t eT
  unless t $ do
    tellCur (LDC 0)
    tellCur (TSEL (Lab lA) (Lab lA))

  tellCur (LABEL lF)
  compExpr t eF
  unless t (tellCur (LABEL lA))

compExpr t (Let binds e2) = do
  let (names, exprs) = unzip binds

  tellCur (DUM (length names))
  fs <- get_frames
  put_frames (names : fs)

  label <- fresh_label "let"
  mapM (compExpr False) exprs

  tellCur (LDF (Lab label))
  tellCur (TRAP (length names))
  tellCur (LABEL label)
  compExpr t e2
compExpr t (CallFun eF eArgs) = do
  mapM (compExpr False) eArgs
  compExpr False eF
  if t
    then tellCur (TAP (length eArgs))
    else tellCur (AP (length eArgs))
compExpr t (Lambda ns e) = do
  label <- fresh_label "lambda"
  fs <- get_frames

  tellCur (LDF (Lab label))

  pass $ do
    put_frames (ns : fs)
    tellCur (LABEL label)
    compExpr True e
    return ((), \(cur, later) -> ([], cur ++ later))

  put_frames fs
  when t (tellCur RTN)

runComp :: CompMonad () -> [Instr]
runComp m =
  let s  = execWriterT m :: State CompState ([Instr], [Instr])
      (cur, later) = evalState s ([["world", "ghosts"]], 0)
  in cur ++ later

runExpr :: Expr -> [Instr]
runExpr  = runComp . compExpr True
