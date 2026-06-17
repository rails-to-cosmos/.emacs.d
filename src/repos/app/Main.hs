module Main where

import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.Aeson (FromJSON, eitherDecode, encode, object, parseJSON, withObject, (.:), (.=))
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (dropTrailingPathSeparator)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import Repos.Discover (findGitRepos)
import Repos.Git (cloneRepo, detectRemote, repoStatus)
import Repos.Table (reposTableView)

data BatchRequest = BatchRequest
  { brCommand :: Text
  , brRepos   :: [FilePath]
  }

instance FromJSON BatchRequest where
  parseJSON = withObject "BatchRequest" $ \o -> BatchRequest
    <$> o .: "command"
    <*> o .: "repos"

newtype TableRequest = TableRequest { trRepos :: [FilePath] }

instance FromJSON TableRequest where
  parseJSON = withObject "TableRequest" $ \o -> TableRequest <$> o .: "repos"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["status", path]        -> BL.putStrLn . encode =<< repoStatus path True
    ["status-quick", path]  -> BL.putStrLn . encode =<< repoStatus path False
    ["discover", dir]       -> BL.putStrLn . encode =<< findGitRepos dir
    ["remote", dir]         -> BL.putStrLn . encode =<< detectRemote dir
    ["clone", remote, dest] -> BL.putStrLn . encode =<< cloneRepo remote dest
    ["batch"]               -> runBatch
    ["table"]               -> runTable
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
        , "  batch                  Read JSON from stdin, stream NDJSON results"
        , "  table                  Read JSON from stdin, emit a TableView (JSON)"
        , ""
        , "batch stdin format:  {\"command\": \"status\"|\"status-quick\", \"repos\": [\"/path\", ...]}"
        , "batch stdout format: one JSON object per line: {\"path\": \"...\", \"status\": {...}}"
        , "table stdin format:  {\"repos\": [\"/path\", ...]}"
        , "table stdout format: a single declarative TableView object"
        ]
      exitFailure

runBatch :: IO ()
runBatch = do
  input <- BL.getContents
  case eitherDecode input of
    Left err -> do
      hPutStrLn stderr $ "batch: invalid JSON: " ++ err
      exitFailure
    Right (BatchRequest cmd paths) -> do
      let doFetch = cmd == "status"
      lock <- newMVar ()
      mapConcurrently_ (processOne lock doFetch) paths

processOne :: MVar () -> Bool -> FilePath -> IO ()
processOne lock doFetch path = do
  status <- repoStatus path doFetch
  withMVar lock $ \_ -> do
    BL.putStrLn $ encode $ object ["path" .= path, "status" .= status]
    hFlush stdout

-- | Gather quick statuses for the given repos and emit one declarative
-- 'Repos.Table.TableView'. Prototype: uses status-quick (no network fetch)
-- so the view renders instantly; the "refresh" action can fetch later.
runTable :: IO ()
runTable = do
  input <- BL.getContents
  case eitherDecode input of
    Left err -> do
      hPutStrLn stderr $ "table: invalid JSON: " ++ err
      exitFailure
    Right (TableRequest paths) -> do
      home <- T.pack . dropTrailingPathSeparator <$> getHomeDirectory
      statuses <- mapConcurrently (\p -> (,) p <$> repoStatus p False) paths
      BL.putStrLn $ encode $ reposTableView home statuses
