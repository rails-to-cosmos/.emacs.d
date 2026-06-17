-- | A generic, declarative table-view schema.
--
-- A backend describes a view by its columns, available actions, default
-- sort and rows; the Emacs @table-view@ core renders any such description
-- and dispatches keys to consumer-registered handlers. Nothing here is
-- repos-specific except 'reposTableView' at the bottom, which is the first
-- consumer — the intent is that this schema graduates into its own package
-- and other dashboards (processes, jobs, timers, ...) reuse it.
module Repos.Table
  ( TableView (..)
  , Column (..)
  , ColType (..)
  , Align (..)
  , Badge (..)
  , Action (..)
  , SortSpec (..)
  , Row (..)
  , reposTableView
  ) where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Aeson.Key (fromText)
import Data.Text (Text)
import Data.Text qualified as T

import Repos.Types

-- | Cell alignment within a column.
data Align = AlignLeft | AlignRight
  deriving (Show, Eq)

instance ToJSON Align where
  toJSON AlignLeft  = "left"
  toJSON AlignRight = "right"

-- | How the core renders (and sorts) a column.
data ColType = ColText | ColNumber | ColBadge
  deriving (Show, Eq)

instance ToJSON ColType where
  toJSON ColText   = "text"
  toJSON ColNumber = "number"
  toJSON ColBadge  = "badge"

-- | A badge palette entry. The list order doubles as sort priority, so it is
-- emitted as a JSON array (not an object, whose key order is not preserved).
data Badge = Badge
  { badgeValue :: Text
  , badgeColor :: Text
  }
  deriving (Show)

instance ToJSON Badge where
  toJSON (Badge v c) = object ["value" .= v, "color" .= c]

data Column = Column
  { colKey      :: Text
  , colHeader   :: Text
  , colType     :: ColType
  , colSortable :: Bool
  , colAlign    :: Align
  , colBadges   :: [Badge] -- ^ empty unless 'colType' is 'ColBadge'
  }
  deriving (Show)

instance ToJSON Column where
  toJSON Column{..} = object
    [ "key"      .= colKey
    , "header"   .= colHeader
    , "type"     .= colType
    , "sortable" .= colSortable
    , "align"    .= colAlign
    , "badges"   .= colBadges
    ]

-- | A key the view binds to a named command. The Elisp consumer maps the
-- command name to an actual function; the backend only declares intent.
data Action = Action
  { actKey     :: Text -- ^ Emacs key description, e.g. "RET", "f", "g"
  , actCommand :: Text -- ^ command name dispatched to the consumer
  , actLabel   :: Text
  }
  deriving (Show)

instance ToJSON Action where
  toJSON Action{..} = object
    [ "key" .= actKey, "command" .= actCommand, "label" .= actLabel ]

data SortSpec = SortSpec
  { sortColumn    :: Text
  , sortAscending :: Bool
  }
  deriving (Show)

instance ToJSON SortSpec where
  toJSON (SortSpec c a) = object ["column" .= c, "ascending" .= a]

data Row = Row
  { rowId    :: Text            -- ^ stable id, passed to action handlers
  , rowCells :: [(Text, Value)] -- ^ column key -> cell value (string or number)
  }
  deriving (Show)

instance ToJSON Row where
  toJSON (Row rid cells) = object
    [ "id"    .= rid
    , "cells" .= object [ fromText k .= v | (k, v) <- cells ]
    ]

data TableView = TableView
  { tvTitle   :: Text
  , tvColumns :: [Column]
  , tvActions :: [Action]
  , tvSort    :: SortSpec
  , tvRows    :: [Row]
  }
  deriving (Show)

instance ToJSON TableView where
  toJSON TableView{..} = object
    [ "title"   .= tvTitle
    , "columns" .= tvColumns
    , "actions" .= tvActions
    , "sort"    .= tvSort
    , "rows"    .= tvRows
    ]

-- ---------------------------------------------------------------------------
-- repos: the first consumer of the generic schema
-- ---------------------------------------------------------------------------

-- | Badge palette for the repo state column. List order is the status sort
-- priority and mirrors the Elisp @repos--todo-order@.
stateBadges :: [Badge]
stateBadges =
  [ Badge "CHECKING"   "#e0af68"
  , Badge "FETCHING"   "#e0af68"
  , Badge "BEHIND"     "#e67e22"
  , Badge "MODIFIED"   "#749AF7"
  , Badge "MISSING"    "#9b59b6"
  , Badge "ERROR"      "#c0392b"
  , Badge "UP_TO_DATE" "#9ece6a"
  , Badge "UNTRACKED"  "#565f89"
  ]

reposColumns :: [Column]
reposColumns =
  [ Column "state"  "State"  ColBadge  True  AlignLeft  stateBadges
  , Column "name"   "Repo"   ColText   True  AlignLeft  []
  , Column "branch" "Branch" ColText   False AlignLeft  []
  , Column "behind" "Behind" ColNumber True  AlignRight []
  , Column "local"  "Local"  ColText   False AlignLeft  []
  ]

reposActions :: [Action]
reposActions =
  [ Action "RET" "open"    "Open in Magit"
  , Action "f"   "pull"    "Pull"
  , Action "g"   "refresh" "Refresh"
  ]

-- | The displayed status keyword for a repo, mirroring Elisp @repos--todo-kw@.
statusKeyword :: RepoStatus -> Text
statusKeyword s = case rsState s of
  Missing  -> "MISSING"
  Checking -> "CHECKING"
  Fetching -> "FETCHING"
  Error    -> "ERROR"
  Ready
    | rsBehind s > 0    -> "BEHIND"
    | rsModified s > 0  -> "MODIFIED"
    | rsUntracked s > 0 -> "UNTRACKED"
    | otherwise         -> "UP_TO_DATE"

-- | Abbreviate a home-relative path to @~/...@ for display.
abbrevHome :: Text -> Text -> Text
abbrevHome home p
  | not (T.null home) && home `T.isPrefixOf` p = "~" <> T.drop (T.length home) p
  | otherwise = p

reposRow :: Text -> (FilePath, RepoStatus) -> Row
reposRow home (path, st) =
  let p = T.pack path
  in Row
       { rowId = p -- absolute: handlers feed this to git/magit
       , rowCells =
           [ ("state",  toJSON (statusKeyword st))
           , ("name",   toJSON (abbrevHome home p))
           , ("branch", toJSON (maybe "" id (rsBranch st)))
           , ("behind", toJSON (rsBehind st))
           , ("local",  toJSON (localCell st))
           ]
       }
  where
    localCell s = case rsState s of
      Error   -> maybe "" id (rsError s)
      Missing -> "clone with c"
      _       -> maybe "" id (rsLocal s)

-- | Build the repos dashboard as a generic 'TableView'. HOME is the absolute
-- home directory (for display abbreviation); ROWS are gathered statuses.
reposTableView :: Text -> [(FilePath, RepoStatus)] -> TableView
reposTableView home rows = TableView
  { tvTitle   = "Repository Status"
  , tvColumns = reposColumns
  , tvActions = reposActions
  , tvSort    = SortSpec "state" True
  , tvRows    = map (reposRow home) rows
  }
