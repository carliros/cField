module Main where

import DataSemFuns
import System.Environment

main :: IO()
main = do args <- getArgs
          if null args
           then putStrLn "Set out list items, eg [30,40,30]"
           else mapM_ (putStrLn . show)$ calculate (concat args)


