module Reporter.Console (reporter) where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (bold, foreground, underline, withGraphics)
import Data.Array as Array
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (error, log)
import Node.Process (setExitCode)
import Reporter (Reporter)

reporter :: { hideSuccess :: Boolean } -> Reporter Effect
reporter { hideSuccess } =
  { error
  , fileResults: \{ filePath, lintResults } ->
      if not $ Array.null lintResults then do
        log $ withGraphics bold filePath
        for_ (Array.nub lintResults) \{ message, sourceRange } ->
          log $ withGraphics (foreground Red) $ "  ✗ " <> (withGraphics (foreground Cyan <> underline) $ filePath <> ":" <> show (sourceRange.start.line + 1) <> ":" <> show (sourceRange.start.column + 1)) <> " - " <> message
      else if not hideSuccess then log $ withGraphics (bold <> (foreground Green)) $ "✓︎ " <> filePath
      else pure unit

  , report: \results -> do
      let { yes: successful, no: failed } = Array.partition (Array.null <<< _.lintResults) results
      log ""
      log $ "Successful: " <> (show $ Array.length successful)
      log $ "Failed: " <> (show $ Array.length failed)
      when (not Array.null failed) $ setExitCode 1
  }

