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
  (modCount, untrackedCount, localDesc, files) <- parsePortcelain path
  let dirty = modCount > 0 || untrackedCount > 0
  pure RepoStatus
    { rsState     = Ready
    , rsBranch    = T.pack <$> branch
    , rsBehind    = behind
    , rsModified  = modCount
    , rsUntracked = untrackedCount
    , rsLocal     = if dirty then Just localDesc else Nothing
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

parsePortcelain :: FilePath -> IO (Int, Int, Text, [Text])
parsePortcelain path = do
  output <- git path ["status", "--porcelain"]
  let ls = maybe [] (filter (not . null) . lines) output
      (modCount, untrackedCount, files) = foldl' classify (0, 0, []) ls
      parts = concat
        [ ["Modified " <> T.pack (show modCount) <> " file" <> plural modCount | modCount > 0]
        , ["Untracked " <> T.pack (show untrackedCount) <> " file" <> plural untrackedCount | untrackedCount > 0]
        ]
      localDesc = T.intercalate ", " parts
  pure (modCount, untrackedCount, localDesc, reverse files)
  where
    classify (m, u, fs) line
      | "?" `isPrefixOfStr` line = (m, u + 1, fileFromLine line : fs)
      | otherwise                = (m + 1, u, fileFromLine line : fs)

    fileFromLine line
      | length line >= 3 = T.pack (drop 3 line)
      | otherwise        = T.pack line

    isPrefixOfStr p s = take (length p) s == p

    plural n = if n == 1 then "" else "s"

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
