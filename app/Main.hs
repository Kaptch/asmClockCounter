module Main where

import Lib
import System.Environment

usage :: IO ()
usage = putStrLn "<filename>"

main :: IO ()
main = do
    fn <- getArgs
    case fn of
        [fn] -> runVerbose fn
        _ -> usage
