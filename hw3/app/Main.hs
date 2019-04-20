module Main where

import System.Environment

import Interpretator
import BaseParser
import Text.Megaparsec

main :: IO ()
main = do
  args  <- getArgs
  let scriptPath = head args
  scriptFile <- readFile scriptPath
  case runParser parseScript "" scriptFile of
    Left e   -> putStrLn (errorBundlePretty e)
    Right script -> do
        env <- runScript script args
        putStrLn (show env)
