module Reporter where

import Prelude

import Rule (Issue)

type FileResults = { filePath :: String, issues :: Array Issue }

type Reporter m =
  { error :: String -> m Unit
  , fileResults :: FileResults -> m Unit
  , report :: Array FileResults -> m Unit
  }
