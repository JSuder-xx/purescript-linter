module CLI.Reporter where

import Prelude

import Data.Time.Duration (Seconds)
import Linter.ModuleRule (Issue)

type FileResults = { filePath :: String, issues :: Array Issue }

type Reporter m =
  { error :: String -> m Unit
  , indicateStarted :: m Unit
  , indicateFileProcessed :: m Unit
  , indicateEnded :: m Unit
  , report :: Seconds -> Array FileResults -> m Unit
  }
