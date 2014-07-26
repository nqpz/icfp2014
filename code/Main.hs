module Main where

import System.Environment
import SuperComp
import LambdaPrint
import LambdaInstrs
import SuperLambda
import Data.List
import SLParse

run = putStrLn . printer . runExpr
runP = putStrLn . printer. fixLabels . runExpr

main :: IO ()
main = do s <- getContents
          let s_ = unlines $ filter (\l ->  not $ isPrefixOf "--" l) $ lines s
          case parseString s_ of
            Left e    -> putStrLn $ show e
            Right prg -> runP prg
