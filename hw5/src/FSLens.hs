{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module FSLens
    ( FS (..)
    , scan
    , cd
    , ls
    , file
    , chngext
    , filenames
    , removeIfEmpty
    ) where

import Control.Lens
import System.FilePath (replaceExtension)
import qualified System.Directory.Tree as D

data FS
    = Dir
    { _name     :: FilePath
    , _contents :: [FS]
    }
    | File
    { _name     :: FilePath
    }
    deriving Show

scan :: FilePath -> IO FS
scan root = mapTree . D.dirTree <$> D.readDirectoryWith (\_ -> return ()) root
  where
    mapTree :: D.DirTree () -> FS
    mapTree (D.File n _)      = File n
    mapTree (D.Dir n content) = Dir n (map mapTree content)
    mapTree (D.Failed _ err)  = error (show err)

makeLenses ''FS
makePrisms ''FS

cd :: String -> Traversal' FS FS
cd path = contents . traversed . filtered (\d -> isn't _File d && (_name d) == path)

ls :: Traversal' FS FilePath
ls = contents . traversed . name

file :: String -> Traversal' FS String
file fname = contents . traversed . filtered (\d -> isn't _Dir d && (_name d) == fname) . name

chngext :: String -> FS -> FS
chngext newExtension =  contents . traversed . filtered (isn't _Dir) . name %~ (`replaceExtension` newExtension)

filenames :: FS -> [FilePath]
filenames fs = fs ^.. ls ++ concatMap filenames (fs ^.. contents . traverse)

removeIfEmpty :: String -> FS -> FS
removeIfEmpty dir root = root & contents %~ filter (\fs -> isn't _Dir fs || (_name fs /= dir) || not (null (fs ^. contents)))