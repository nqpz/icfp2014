-- hybrid between low-level and mid-level

IKKE FÆRDIG
import Data.Maybe
import qualified LambdaMan as LM

type Label = String

data Instr = LMInstr LM.Instr -- base instruction
           | Label Label
           | IfThenElseGoto Label Label

instrAndPosToLabel :: (Instr, Int) -> Maybe (Name, Int)
instrAndPosToLabel (Label l, n) = Just (l, n)

findLabels :: [Instr] -> [(Label, Int)]
findLabels = mapMaybe instrAndPosToLabel . zip [0..]

compile :: [Instr] -> LMExtM [LM.Instr]
compile instrs = findLabels instrs
