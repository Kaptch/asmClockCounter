module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    [fn] <- getArgs
    result <- countClocksFromParsedFile fn
    print result
    return ()
