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
findLabels n (i : is) = i : findLabels (n+1) is

unLab :: LabelMap -> Addr -> Int
unLab _ (Abs n) = n
unLab m (Lab s) =
  case lookup s m of
    Just n  -> n
    Nothing -> error ("Undefined label " ++ s)

removeLabels :: LabelMap -> [Instr] -> [Instr]
removeLabels _ []                = []
removeLabels _ (LABEL _ : is)    = error "Impossible: Found label"
removeLabels m (SEL a1 a2 : is)  = SEL  (unLab m a1) (unLab m a2) : removeLabels m is
removeLabels m (TSEL a1 a2 : is) = TSEL (unLab m a1) (unLab m a2) : removeLabels m is
removeLabels m (LDR a : is)      = LDR  (unLab m a) : removeLabels m is
removeLabels m (i : is)          = i : removeLabels m is
