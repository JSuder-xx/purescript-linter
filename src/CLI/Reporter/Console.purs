module CLI.Reporter.Console (reporter) where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (bold, foreground, underline, withGraphics)
import CLI.Reporter (Reporter)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Newtype (un)
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Effect.Console (error, log)
import Node.Process (setExitCode)

reporter :: { hideSuccess :: Boolean } -> Reporter Effect
reporter { hideSuccess } =
  { error
  , fileResults: \{ filePath, issues } ->
      if not $ Array.null issues then do
        log $ withGraphics bold filePath
        for_ (Array.nub issues) \{ message, sourceRange } ->
          log $ withGraphics (foreground Red) $ "  ✗ " <> (withGraphics (foreground Cyan <> underline) $ filePath <> ":" <> show (sourceRange.start.line + 1) <> ":" <> show (sourceRange.start.column + 1)) <> " - " <> message
      else if not hideSuccess then log $ withGraphics (bold <> (foreground Green)) $ "✓︎ " <> filePath
      else pure unit

  , report: \duration results -> do
      let { yes: successful, no: failed } = Array.partition (Array.null <<< _.issues) results
      log ""
      if Array.null failed then
        log $ withGraphics (bold <> (foreground Green)) $ "✓︎ " <> (show $ Array.length successful) <> " file(s) linted successfully!!!"
      else do
        log $ "Successful: " <> (show $ Array.length successful)
        log $ "Failed: " <> (show $ Array.length failed)
      log $ "Lint Time: " <> (show $ un Seconds duration) <> " seconds"
      when (not Array.null failed) $ setExitCode 1
  }
