module Interpretator
    ( runScript
    , ScriptEnvironment
    ) where

import BaseParser(StatementValue(..), Script, Statement(..))

import qualified Data.Map as Map
import Data.List(intercalate, isPrefixOf)
import Data.Maybe(fromMaybe)
import Data.Char(isSpace)


import Control.Monad.Reader

type ScriptEnvironment = Map.Map String String

runScript :: Script -> [String] -> IO (ScriptEnvironment)
runScript script args = do
    let arguments = Map.fromList $ mapInd (\x idx -> (show idx, x)) args
    (env, _ ) <- runReaderT (interpret script) arguments
    return (env)

interpret :: Script -> ReaderT ScriptEnvironment IO (ScriptEnvironment, ())
interpret [] = do
    enviroment <- ask
    return (enviroment, ())

interpret ((Assign var statements):xs) = do
    enviroment <- ask
    let resolved = resolveStatementValue enviroment statements
    let resolver = printResolved resolved
    local (\env -> Map.insert var resolver env) (interpret xs)

interpret ((Command cmd arguments): xs) = do
     enviroment <- ask
     let resolvedArguments = fmap (resolveStatementValue enviroment) arguments
     let printedArguments = fmap printResolved resolvedArguments
     newEnv <- execute cmd printedArguments
     local (\_ -> newEnv) (interpret xs)

resolveStatementValue :: ScriptEnvironment -> [StatementValue] -> [StatementValue]
resolveStatementValue variables = interpretInternal
 where
    interpretInternal [] = []
    interpretInternal (Text x: xs) =  (Text x) : resolveStatementValue variables xs
    interpretInternal (Reference x:xs) = (Text $ fromMaybe mempty (Map.lookup x variables)) : resolveStatementValue variables xs

{-
execute :: String -> [String] -> IO (Int, String, String)
execute cmd args = do
    (exCode, out, err) <- Except.catch (readCreateProcessWithExitCode (proc (externalName cmd) v) "") errorHandler
    case exCode of
        ExitSuccess -> pure (0, out, me)
        ExitFailure code -> pure (code, err, me)

-}

execute :: String -> [String] -> ReaderT ScriptEnvironment IO (ScriptEnvironment)
execute "echo" args = do
    env <- ask
    let firstArg = dropWhile (isSpace) (head args)
    if ("-n" `isPrefixOf` firstArg)
    then do
        let outputStr = intercalate " " (tail args)
        lift $ putStr outputStr
    else do
        let outputStr = intercalate " " args
        lift $ putStrLn outputStr
    return (env)

execute "read" args = do
    line <- lift $ getLine
    let vars = Map.fromList $ safeZip args (words line)
    env <- ask
    return (Map.union env vars)

execute cmd args = error ("Unsupported command " ++ cmd)

printResolved :: [StatementValue] -> String
printResolved [] = ""
printResolved (Text x: xs) = x <> printResolved xs
printResolved (x: xs) = (show x) <> printResolved xs

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

safeZip :: [String] -> [String] -> [(String, String)]
safeZip [] [] = []
safeZip (k:ks) [] = (k, "") : safeZip ks []
safeZip (k:ks) (v:vs) = (k, v) : safeZip ks vs