module Repos.Types where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)

data RepoState
  = Ready
  | Missing
  | Error
  | Fetching
  | Checking
  deriving (Show, Eq)

instance ToJSON RepoState where
  toJSON Ready    = "ready"
  toJSON Missing  = "missing"
  toJSON Error    = "error"
  toJSON Fetching = "fetching"
  toJSON Checking = "checking"

data RepoStatus = RepoStatus
  { rsState     :: RepoState
  , rsBranch    :: Maybe Text
  , rsBehind    :: Int
  , rsStaged    :: Int  -- ^ files with index-side changes (git porcelain X)
  , rsModified  :: Int  -- ^ files with worktree-side changes (git porcelain Y)
  , rsUntracked :: Int
  , rsConflicts :: Int  -- ^ unmerged files
  , rsFiles     :: [Text]
  , rsError     :: Maybe Text
  } deriving (Show)

instance ToJSON RepoStatus where
  toJSON RepoStatus{..} = object
    [ "state"     .= rsState
    , "branch"    .= rsBranch
    , "behind"    .= rsBehind
    , "staged"    .= rsStaged
    , "modified"  .= rsModified
    , "untracked" .= rsUntracked
    , "conflicts" .= rsConflicts
    , "files"     .= rsFiles
    , "error"     .= rsError
    ]

emptyStatus :: RepoStatus
emptyStatus = RepoStatus
  { rsState     = Checking
  , rsBranch    = Nothing
  , rsBehind    = 0
  , rsStaged    = 0
  , rsModified  = 0
  , rsUntracked = 0
  , rsConflicts = 0
  , rsFiles     = []
  , rsError     = Nothing
  }

errorStatus :: Text -> RepoStatus
errorStatus msg = emptyStatus { rsState = Error, rsError = Just msg }

missingStatus :: RepoStatus
missingStatus = emptyStatus { rsState = Missing }
