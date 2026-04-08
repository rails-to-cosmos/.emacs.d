module Repos.Discover (findGitRepos) where

import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

-- | Directories to skip during recursive discovery.
skipDirs :: [String]
skipDirs = [".git", "node_modules", ".cache", ".stack-work", "dist-newstyle"]

-- | Recursively find all git repositories under a directory.
-- Does not descend into repos (stops at .git).
findGitRepos :: FilePath -> IO [Text]
findGitRepos dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      isRepo <- doesDirectoryExist (dir </> ".git")
      if isRepo
        then pure [T.pack dir]
        else do
          entries <- listDirectory dir
          repos <- concat <$> mapM (processEntry dir) entries
          pure repos

processEntry :: FilePath -> String -> IO [Text]
processEntry parent name
  | name `elem` skipDirs = pure []
  | head name == '.'     = pure []
  | otherwise = do
      let path = parent </> name
      isDir <- doesDirectoryExist path
      if isDir
        then findGitRepos path
        else pure []
