module Repos.Git
  ( repoStatus
  , detectRemote
  , cloneRepo
  ) where

import Control.Exception (SomeException, try)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

import Repos.Types

-- | Run a git command in a directory, return trimmed stdout or Nothing on failure.
git :: FilePath -> [String] -> IO (Maybe String)
git dir args = do
  result <- try @SomeException $ readProcessWithExitCode "git" ("-C" : dir : args) ""
  pure $ case result of
    Right (ExitSuccess, out, _) -> Just (trimStr out)
    _                           -> Nothing

trimStr :: String -> String
trimStr = reverse . dropWhile (== '\n') . reverse . dropWhile (== '\n')

-- | Gather full status for a repo: fetch, then branch + porcelain + behind count.
repoStatus :: FilePath -> Bool -> IO RepoStatus
repoStatus path doFetch = do
  dirExists <- doesDirectoryExist path
  if not dirExists
    then pure missingStatus
    else do
      gitExists <- doesDirectoryExist (path </> ".git")
      if not gitExists
        then pure (errorStatus "Not a git repo")
        else do
          when' doFetch $ do
            _ <- git path ["fetch", "--quiet"]
            pure ()
          gatherStatus path

when' :: Bool -> IO () -> IO ()
when' True  a = a
when' False _ = pure ()

gatherStatus :: FilePath -> IO RepoStatus
gatherStatus path = do
  branch  <- git path ["rev-parse", "--abbrev-ref", "HEAD"]
  behind  <- parseBehind <$> git path ["rev-list", "--count", "HEAD..@{u}"]
  (staged, modCount, untrackedCount, conflicts, files) <- parsePorcelain path
  pure RepoStatus
    { rsState     = Ready
    , rsBranch    = T.pack <$> branch
    , rsBehind    = behind
    , rsStaged    = staged
    , rsModified  = modCount
    , rsUntracked = untrackedCount
    , rsConflicts = conflicts
    , rsFiles     = files
    , rsError     = Nothing
    }

parseBehind :: Maybe String -> Int
parseBehind Nothing  = 0
parseBehind (Just s) = fromMaybe 0 (readMaybe' s)

readMaybe' :: String -> Maybe Int
readMaybe' s = case reads s of
  [(n, "")] -> Just n
  _         -> Nothing

-- | Count files by @git status --porcelain@ XY status codes.
-- Returns (staged, modified, untracked, conflicts, files).  A file may be
-- counted in both staged and modified (e.g. code @MM@); untracked (@??@) and
-- unmerged files are counted only in their own bucket.
parsePorcelain :: FilePath -> IO (Int, Int, Int, Int, [Text])
parsePorcelain path = do
  output <- git path ["status", "--porcelain"]
  let ls = maybe [] (filter (not . null) . lines) output
      (staged, modified, untracked, conflicts, files) =
        foldl' classify (0, 0, 0, 0, []) ls
  pure (staged, modified, untracked, conflicts, reverse files)
  where
    classify (s, m, u, c, fs) line =
      let x    = charAt line 0
          y    = charAt line 1
          code = [x, y]
          fs'  = fileFromLine line : fs
      in if code == "??"            then (s, m, u + 1, c, fs')
         else if code `elem` unmerged then (s, m, u, c + 1, fs')
         else ( if x `elem` indexCodes then s + 1 else s
              , if y `elem` treeCodes  then m + 1 else m
              , u, c, fs' )

    -- The seven unmerged combinations git reports (both sides `U`, or the
    -- add/add and delete/delete cases).
    unmerged :: [String]
    unmerged = ["DD", "AU", "UD", "UA", "DU", "AA", "UU"]

    indexCodes = "MADRC" :: String  -- index side has a staged change
    treeCodes  = "MD"    :: String  -- worktree side has an unstaged change

    charAt s i = if length s > i then s !! i else ' '

    fileFromLine line
      | length line >= 3 = T.pack (drop 3 line)
      | otherwise        = T.pack line

-- | Detect the origin remote URL for a git repo.
detectRemote :: FilePath -> IO (Maybe Text)
detectRemote path = do
  result <- git path ["remote", "get-url", "origin"]
  pure $ case result of
    Just s | not (null s) -> Just (T.pack s)
    _                     -> Nothing

-- | Clone a remote repo to a target directory.
cloneRepo :: String -> FilePath -> IO RepoStatus
cloneRepo remote target = do
  result <- try @SomeException $
    readProcessWithExitCode "git" ["clone", remote, target] ""
  case result of
    Right (ExitSuccess, _, _) -> repoStatus target False
    Right (_, _, err)         -> pure (errorStatus $ "Clone failed: " <> T.pack err)
    Left e                    -> pure (errorStatus $ "Clone failed: " <> T.pack (show e))
