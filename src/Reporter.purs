module Reporter where

import Prelude

import Linter (LintResults)

type FileResults = { filePath :: String, lintResults :: LintResults }

type Reporter m =
  { error :: String -> m Unit
  , fileResults :: FileResults -> m Unit
  , report :: Array FileResults -> m Unit
  }
