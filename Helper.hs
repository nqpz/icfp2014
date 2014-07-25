import SuperComp
import LambdaPrint
import LambdaInstrs
import SuperLambda


run = putStrLn . printer . runExpr
runP = putStrLn . printer. fixLabels . runExpr
