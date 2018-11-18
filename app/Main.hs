module Main where

import Lib
import System.Environment

usage :: IO ()
usage = do
    putStrLn "<filename> [options]"
    putStrLn "options: -v, --verbose"

main :: IO ()
main = do
    fn <- getArgs
    case fn of
        [fn] -> print <$> countClocksFromParsedFile fn *> return ()
        [fn, "-v"] -> runVerbose fn
        [fn, "--verbose"] -> runVerbose fn
        _ -> usage
