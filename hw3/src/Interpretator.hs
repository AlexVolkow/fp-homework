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
import System.Directory (canonicalizePath, doesPathExist, getCurrentDirectory)
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

interpret ((Unknown _) : xs) = do
    local id (interpret xs)

interpret ((Assign var statements):xs) = do
    state <- ask
    enviroment <- lift $ readIORef state
    let vars = ctxVars enviroment
    let resolved = resolveStatementValue vars statements
    let extracted = (pure resolved) >>= (extractInnerCalls)
    ans <- extracted
    let resolver = printResolved ans
    lift $ modifyIORef state (setVars (Map.insert var resolver vars))
    local (const state) (interpret xs)

interpret ((Command cmd arguments): xs) = do
    (out, newline) <- executeCommand cmd arguments
    if (newline)
    then
        lift $ putStrLn out
    else
        lift $ putStr out
    local id (interpret xs)

resolveStatementValue :: Map.Map String String -> StatementValue -> StatementValue
resolveStatementValue variables = resolveInternal
 where
    resolveInternal [] = []
    resolveInternal (Text x: xs) =  (Text x) : resolveInternal xs
    resolveInternal (Reference x:xs) = (Text $ fromMaybe "" (Map.lookup x variables)) : resolveInternal xs
    resolveInternal ((InnerCall cmds):xs) = (InnerCall (map (\cmd -> resolveStatement variables cmd) cmds)) : resolveInternal xs

resolveStatement :: Map.Map String String -> Statement -> Statement
resolveStatement variables = resolveInternal
 where
    resolveInternal (Assign var value) =  (Assign var (resolveStatementValue variables value))
    resolveInternal (Command name args) = (Command name (map (\arg -> resolveStatementValue variables arg) args))
    resolveInternal (Unknown value) = (Unknown (resolveStatementValue variables value))

extractInnerCalls :: StatementValue -> ReaderT ScriptEnvironment IO (StatementValue)
extractInnerCalls = traverse extractInnerCall

extractInnerCall :: StatementToken -> ReaderT ScriptEnvironment IO (StatementToken)
extractInnerCall (InnerCall cmds) = fmap (\e -> Text e) (go cmds)
    where
        extractInternal (Command cmd args) = do
            rargs <- traverse extractInnerCalls args
            liftM (\(out, _) -> trim out) (executeInnerCommand cmd rargs)

        extractInternal (Assign var _) = return $ "unsupported assgin " ++ var
        extractInternal (Unknown _) = return $ "unknown"

        go [] = return []
        go (x : []) = do
            e <- extractInternal x
            return e
        go (x: xs) = do
            e <- extractInternal x
            t <- go xs
            return $ e ++ " " ++ t

extractInnerCall a = return (a)

executeInnerCommand :: String -> [StatementValue] -> ReaderT ScriptEnvironment IO (String, Bool)
executeInnerCommand cmd arguments = do
    state <- ask
    liftIO $ E.catch (runReaderT (executeCommand cmd arguments) state) innerCallErrorHandler

executeCommand :: String -> [StatementValue] -> ReaderT ScriptEnvironment IO (String, Bool)
executeCommand cmd arguments = do
    state <- ask
    enviroment <- lift $ readIORef state
    let vars = ctxVars enviroment
    let resolvedArguments = fmap (resolveStatementValue vars) arguments
    let extracted = traverse extractInnerCalls resolvedArguments
    ans <- extracted
    let printedArguments = fmap printResolved ans
    execute cmd printedArguments

execute :: String -> [String] -> ReaderT ScriptEnvironment IO (String, Bool)
execute "echo" [] = do
    return ("", False)

execute "echo" args = do
    let firstArgs = words (head args)
    if ((length firstArgs == 1) && "-n" == (head firstArgs))
    then do
        let outputStr = intercalate " " (filter (not . null) (intercalate " " (tail firstArgs) : (tail args)))
        return (outputStr, False)
    else do
        let outputStr = intercalate " " args
        return (outputStr, True)

execute "read" args =
    case checkReadArguments(args) of
        Just arg -> error ("invalid variable name " ++ arg)
        Nothing -> do
            line <- lift $ getLine
            let vars = Map.fromList $ safeZip args (words line)
            env <- ask
            envVars <- lift $ readIORef env
            lift $ modifyIORef env (setVars (Map.union (ctxVars envVars) vars))
            return ("", False)

execute "pwd" _ = do
    state <- ask
    enviroment <- lift $ readIORef state
    let path = ctxPath enviroment
    return (path, True)

execute "cd" args = do
    let filePath = intercalate "" args
    state <- ask
    enviroment <- lift $ readIORef state
    let newPath = (ctxPath enviroment) </> filePath
    exist <- lift $ doesPathExist newPath
    canonPath <- lift $ canonicalizePath newPath
    if (exist)
    then do
        lift $ modifyIORef state (setPath canonPath)
        return ("", False)
    else
        return ("Path does not exist " ++ canonPath, True)

execute "exit" args = do
    let code = read (head args)
    if (code == 0)
    then
        lift $ exitWith ExitSuccess
    else
        lift $ exitWith (ExitFailure code)

execute cmd args = do
    (exCode, out, err) <- lift $ executeExternal cmd args
    case exCode of
        ExitSuccess -> return (out, False)
        ExitFailure code -> return ("Command '" ++ cmd ++ "' returned code " ++ (show code)
            ++ (concatIfNotNull ", message :" err), True)

checkReadArguments :: [String] -> Maybe String
checkReadArguments = find (not . valid)
    where
        valid []      = True
        valid (x: xs) = isLetter x && all isAlphaNum xs

executeExternal :: String -> [String] -> IO (ExitCode, String, String)
executeExternal cmd args = E.catch (readCreateProcessWithExitCode (proc cmd args) "") errorHandler

errorHandler :: E.SomeException -> IO (ExitCode, String, String)
errorHandler _ = pure (ExitFailure 1, mempty, mempty)

innerCallErrorHandler :: E.SomeException -> IO (String, Bool)
innerCallErrorHandler _ = pure ("", False)

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

concatIfNotNull :: String -> String -> String
concatIfNotNull arg1 arg2 = if (null arg2) then "" else arg1 ++ arg2

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
