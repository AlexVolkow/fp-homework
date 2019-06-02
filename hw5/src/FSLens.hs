{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module FSLens
    ( FS (..)
    , getDirectoryFs
    , cd
    , ls
    , file
    , chngext
    , filenames
    , removeIfEmpty
    ) where

import Lens.Micro (Lens', lens, Traversal', (%~), (^.), (^..), (&), filtered, traversed)
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

getDirectoryFs :: FilePath -> IO FS
getDirectoryFs root = mapTree . D.dirTree <$> D.readDirectoryWith (\_ -> return ()) root
  where
    mapTree :: D.DirTree () -> FS
    mapTree (D.File n _)      = File n
    mapTree (D.Dir n content) = Dir n (map mapTree content)
    mapTree (D.Failed _ err)  = error (show err)

name :: Lens' FS FilePath
name = lens getName setName
  where
    getName (Dir n _) = n
    getName (File n) = n

    setName (Dir _ c) path = Dir path c
    setName (File _) path = File path

contents :: Lens' FS [FS]
contents = lens getContent setContent
 where
    getContent (Dir _ c) = c
    getContent (File n) = []

    setContent (Dir n _) content = Dir n content
    setContent (File n) _ = error $ (show n) ++ " isn't a directory"

isFile :: FS -> Bool
isFile (File _) = True
isFile _        = False

isDir :: FS -> Bool
isDir (Dir _ _) = True
isDir _         = False

cd :: String -> Traversal' FS FS
cd path = contents . traversed . filtered (\d -> isDir d && d ^. name == path)

ls :: Traversal' FS FilePath
ls = contents . traversed . name

file :: String -> Traversal' FS String
file fname = contents . traversed . filtered (\d -> isFile d && d ^. name == fname) . name

chngext :: String -> FS -> FS
chngext newExt =  contents . traversed . filtered isFile . name %~ (`replaceExtension` newExt)

filenames :: FS -> [FilePath]
filenames fs = fs ^.. ls ++ concatMap filenames (fs ^.. contents . traverse)

removeIfEmpty :: String -> FS -> FS
removeIfEmpty dir root = root & contents %~ filter (\fs -> isFile fs || (fs ^. name /= dir) || not (null (fs ^. contents)))
