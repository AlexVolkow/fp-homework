module Main where

import System.Environment (getArgs)

import Interpretator (runScript)
import ShParser (parseScript)
import Text.Megaparsec (errorBundlePretty, runParser)

main :: IO ()
main = do
  args <- getArgs
  if ((not . null) args)
  then do
      let scriptPath = head args
      scriptFile <- readFile scriptPath
      case runParser parseScript "" scriptFile of
        Left e   -> putStrLn (errorBundlePretty e)
        Right script -> do
            env <- runScript script args
            putStrLn ""
            putStrLn (show env)
  else
    return ()

