module Main where

import SuperComp
import LambdaPrint
import LambdaInstrs
import SuperLambda
import SLParse

run = putStrLn . printer . runExpr
runP = putStrLn . printer. fixLabels . runExpr

main :: IO ()
main = do prg <- parseFile "test.sl"
          case prg of
            Left e    -> putStrLn $ show e
            Right prg -> runP prg
