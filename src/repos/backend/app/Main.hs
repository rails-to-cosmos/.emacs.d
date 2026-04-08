module Main where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Repos.Discover (findGitRepos)
import Repos.Git (cloneRepo, detectRemote, repoStatus)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["status", path]        -> BL.putStrLn . encode =<< repoStatus path True
    ["status-quick", path]  -> BL.putStrLn . encode =<< repoStatus path False
    ["discover", dir]       -> BL.putStrLn . encode =<< findGitRepos dir
    ["remote", dir]         -> BL.putStrLn . encode =<< detectRemote dir
    ["clone", remote, dest] -> BL.putStrLn . encode =<< cloneRepo remote dest
    _ -> do
      hPutStrLn stderr $ unlines
        [ "Usage: repos-backend <command> [args]"
        , ""
        , "Commands:"
        , "  status <path>          Fetch and report status (JSON)"
        , "  status-quick <path>    Report status without fetching (JSON)"
        , "  discover <dir>         Find git repos recursively (JSON)"
        , "  remote <dir>           Detect origin remote URL (JSON)"
        , "  clone <remote> <dest>  Clone and report status (JSON)"
        ]
      exitFailure
