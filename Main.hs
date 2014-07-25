module Main where

import System.Environment
import SuperComp
import LambdaPrint
import LambdaInstrs
import SuperLambda
import SLParse

run = putStrLn . printer . runExpr
runP = putStrLn . printer. fixLabels . runExpr

main :: IO ()
main = do args <- getArgs
          prg <- parseFile (head args)
          case prg of
            Left e    -> putStrLn $ show e
            Right prg -> runP prg
