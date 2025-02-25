module CLI.Reporter.Console (reporter) where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (bold, foreground, underline, withGraphics)
import CLI.AppConfig (Verbosity(..))
import CLI.Reporter (Reporter)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Newtype (un)
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Effect.Console (error, log)
import Node.Encoding (Encoding(..))
import Node.Process (setExitCode, stdout)
import Node.Stream (writeString)

reporter :: { verbosity :: Verbosity } -> Reporter Effect
reporter { verbosity } =
  { error
  , indicateStarted: case verbosity of
      Quiet -> pure unit
      Brief -> void $ writeString stdout ASCII "Linting"
      Verbose -> log "Linting"
  , indicateFileProcessed: \{ filePath, issues } -> case verbosity of
      Quiet -> pure unit
      Brief -> void $ writeString stdout ASCII $ if Array.null issues then "." else "X"
      Verbose -> log
        if Array.null issues then (withGraphics (bold <> (foreground Green)) "✓︎ ") <> filePath
        else (withGraphics (bold <> (foreground Red)) "✗ ") <> filePath
  , indicateEnded: case verbosity of
      Quiet -> pure unit
      _ -> log ""
  , report: \duration results -> do
      let { yes: successful, no: failed } = Array.partition (Array.null <<< _.issues) results
      if (verbosity == Quiet) then for_ failed emitIssues
      else do
        log ""
        if Array.null failed then
          log $ withGraphics (bold <> (foreground Green)) $ "✓︎ " <> (show $ Array.length successful) <> " file(s) linted successfully!!!"
        else do
          for_ failed emitIssues
          log $ "Successful: " <> (show $ Array.length successful)
          log $ "Failed: " <> (show $ Array.length failed)
        log $ "Lint Time: " <> (show $ un Seconds duration) <> " seconds"
        when (not Array.null failed) $ setExitCode 1
  }
  where
  emitIssues { filePath, issues } = do
    log $ withGraphics bold filePath
    for_ (Array.nub issues) \{ message, sourceRange } ->
      log $ withGraphics (foreground Red) $ "  ✗ " <> (withGraphics (foreground Cyan <> underline) $ filePath <> ":" <> show (sourceRange.start.line + 1) <> ":" <> show (sourceRange.start.column + 1)) <> " - " <> message
