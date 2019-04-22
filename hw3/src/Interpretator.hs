module Interpretator
    ( runScript
    , interpret
    , ScriptEnvironment
    , ScriptEnvironmentImpl(..)
    ) where

import ShParser (Script, Statement (..), StatementToken (..), StatementValue)

import qualified Control.Exception as E
import qualified Data.Map as Map

import Control.Monad.Reader
import Data.Char (isAlphaNum, isLetter, isSpace)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe)
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath.Posix ((</>))
import System.Process (proc, readCreateProcessWithExitCode)

data ScriptEnvironmentImpl = ScriptEnvironmentImpl
    { ctxVars :: Map.Map String String
    , ctxPath :: FilePath
    } deriving (Show)

type ScriptEnvironment = IORef ScriptEnvironmentImpl

runScript :: Script -> [String] -> IO (ScriptEnvironmentImpl)
runScript script args = do
    let arguments = Map.fromList $ mapInd (\x idx -> (show idx, x)) args
    dir <- getCurrentDirectory
    scriptEnvironment <- newIORef $ ScriptEnvironmentImpl { ctxVars = arguments, ctxPath = dir}
    runReaderT (interpret script) scriptEnvironment
    env <- readIORef scriptEnvironment
    return (env)

interpret :: Script -> ReaderT ScriptEnvironment IO ()
interpret [] = do
    return ()

interpret ((Assign var statements):xs) = do
    state <- ask
    enviroment <- lift $ readIORef state
    let vars = ctxVars enviroment
    let resolved = resolveStatementValue vars statements
    let resolver = printResolved resolved
    lift $ modifyIORef state (setVars (Map.insert var resolver vars))
    local (const state) (interpret xs)

interpret ((Command cmd arguments): xs) = do
    state <- ask
    enviroment <- lift $ readIORef state
    let vars = ctxVars enviroment
    let resolvedArguments = fmap (resolveStatementValue vars) arguments
    let printedArguments = fmap printResolved resolvedArguments
    execute cmd printedArguments
    local (const state) (interpret xs)

resolveStatementValue :: Map.Map String String -> StatementValue -> StatementValue
resolveStatementValue variables = interpretInternal
 where
    interpretInternal [] = []
    interpretInternal (Text x: xs) =  (Text x) : interpretInternal xs
    interpretInternal (Reference x:xs) = (Text $ fromMaybe "" (Map.lookup x variables)) : interpretInternal xs

execute :: String -> [String] -> ReaderT ScriptEnvironment IO ()
execute "echo" [] = do
    return ()

execute "echo" args = do
    let firstArgs = words (head args)
    if ((length firstArgs == 1) && "-n" == (head firstArgs))
    then do
        let outputStr = intercalate " " (filter (not . null) (intercalate " " (tail firstArgs) : (tail args)))
        lift $ putStr outputStr
    else do
        let outputStr = intercalate " " args
        lift $ putStrLn outputStr
    return ()

execute "read" args =
    case checkReadArguments(args) of
        Just arg -> error ("invalid variable name " ++ arg)
        Nothing -> do
            line <- lift $ getLine
            let vars = Map.fromList $ safeZip args (words line)
            env <- ask
            envVars <- lift $ readIORef env
            lift $ modifyIORef env (setVars (Map.union (ctxVars envVars) vars))

execute "pwd" args = do
    state <- ask
    enviroment <- lift $ readIORef state
    abs <- lift $ canonicalizePath (ctxPath enviroment)
    lift $ putStrLn abs
    lift $ modifyIORef state (setPath abs)

execute "cd" args = do
    let filePath = intercalate "" args
    state <- ask
    enviroment <- lift $ readIORef state
    let newPath = (ctxPath enviroment) </> filePath
    lift $ modifyIORef state (setPath newPath)

execute "exit" args = do
    let code = read (head args)
    if (code == 0)
    then
        lift $ exitWith ExitSuccess
    else
        lift $ exitWith (ExitFailure code)

execute cmd args = error ("Unsupported command " ++ cmd)

checkReadArguments :: [String] -> Maybe String
checkReadArguments = find (not . valid)
    where
        valid []      = True
        valid (x: xs) = isLetter x && all isAlphaNum xs


executeExternal :: String -> [String] -> IO (ExitCode, String, String)
executeExternal cmd args = E.catch (readCreateProcessWithExitCode (proc cmd args) "") errorHandler

errorHandler :: E.SomeException -> IO (ExitCode, String, String)
errorHandler _ = pure (ExitFailure 1, mempty, mempty)

printResolved :: StatementValue -> String
printResolved []           = ""
printResolved (Text x: xs) = x <> printResolved xs
printResolved (x: xs)      = (show x) <> printResolved xs

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

safeZip :: [String] -> [String] -> [(String, String)]
safeZip [] _          = []
safeZip (k:[]) l      = [(k,intercalate " " l)]
safeZip (k:ks) []     = (k, "") : safeZip ks []
safeZip (k:ks) (v:vs) = (k, v) : safeZip ks vs

setPath :: FilePath -> ScriptEnvironmentImpl -> ScriptEnvironmentImpl
setPath newPath (ScriptEnvironmentImpl vars _) = ScriptEnvironmentImpl {ctxVars = vars, ctxPath = newPath}

setVars :: Map.Map String String -> ScriptEnvironmentImpl -> ScriptEnvironmentImpl
setVars newVars (ScriptEnvironmentImpl _ path)  = ScriptEnvironmentImpl {ctxVars = newVars, ctxPath = path}

