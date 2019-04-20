module Interpretator
    ( runScript
    , ScriptEnvironment
    ) where

import BaseParser(StatementValue(..), Script, Statement(..))

import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Maybe(fromMaybe)

type ScriptEnvironment = Map.Map String String

runScript :: Script -> [String] -> IO (ScriptEnvironment)
runScript script args = do
    let arguments = Map.fromList $ mapInd (\x idx -> (show idx, x)) args
    (env, _ ) <- runReaderT (interpret script) arguments
    return (env)

interpret :: Script -> ReaderT ScriptEnvironment IO (ScriptEnvironment, ())
interpret [] = do
    currentState <- ask
    lift $ putStrLn "OK"
    return (currentState, ())

interpret ((Assign var statements):xs) = do
    currentState <- ask
    let resolver = interpretAssign currentState statements
    let updMap = Map.insert var resolver currentState
    lift $ putStrLn (var ++ "=" ++ resolver)
    local (\_ -> updMap) (interpret xs)

interpretAssign :: ScriptEnvironment -> [StatementValue] -> String
interpretAssign variables = interpretInternal
 where
    interpretInternal [] = ""
    interpretInternal (Text x: xs) = "\"" ++  x  ++ "\"" <> interpretAssign variables xs
    interpretInternal (Reference x:xs) = fromMaybe mempty (Map.lookup x variables) <> interpretAssign variables xs

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]