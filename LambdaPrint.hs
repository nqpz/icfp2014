module LambdaPrint (fixLabels, printer) where

import LambdaInstrs

type LabelMap = [(Label, Int)]

fixLabels :: [Instr] -> [Instr]
fixLabels is =
  let (is', labels) = findLabels 0 is
  in removeLabels labels is'

findLabels :: Int -> [Instr] -> ([Instr], LabelMap)
findLabels _ [] = ([], [])
findLabels n (LABEL s : is) =
  let (is', labels) = findLabels n is
  in (is', (s, n) : labels)
findLabels n (i : is) =
  let (is', labels) = findLabels (n+1) is
  in (i : is', labels)

unLab :: LabelMap -> Addr -> Addr
unLab _ (Abs n) = Abs n
unLab m (Lab s) =
  case lookup s m of
    Just n  -> Abs n
    Nothing -> error ("Undefined label " ++ s)

removeLabels :: LabelMap -> [Instr] -> [Instr]
removeLabels _ []                = []
removeLabels _ (LABEL _ : is)    = error "Impossible: Found label"
removeLabels m (SEL a1 a2 : is)  = SEL  (unLab m a1) (unLab m a2) : removeLabels m is
removeLabels m (TSEL a1 a2 : is) = TSEL (unLab m a1) (unLab m a2) : removeLabels m is
removeLabels m (LDF a : is)      = LDF  (unLab m a) : removeLabels m is
removeLabels m (i : is)          = i : removeLabels m is

printer :: [Instr] -> String
printer = unlines . map printer'

printer' :: Instr -> String
printer' (LD  (a, b)) = "LD "   ++ show a ++ ", " ++ show b
printer' (ST  (a, b)) = "ST "   ++ show a ++ ", " ++ show b
printer' (SEL  a  b)  = "SEL "  ++ show a ++ ", " ++ show b
printer' (TSEL a  b)  = "TSEL " ++ show a ++ ", " ++ show b
printer' (LABEL s    ) = s ++ ":"
printer' i             = show i
