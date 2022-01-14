module Main where

import qualified MyLib (someFunc)
import Relude

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
