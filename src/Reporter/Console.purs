module Reporter.Console (reporter) where

import Prelude

import Ansi.Codes (GraphicsParam, escapeCodeToString)
import Ansi.Codes as AnsiCode
import Data.Array as Array
import Data.Foldable (for_)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Console (error, log)
import Reporter (Reporter)

reporter :: { hideSuccess :: Boolean } -> Reporter Effect
reporter { hideSuccess } =
  { error
  , fileResults: \{ filePath, lintResults } ->
      if not $ Array.null lintResults then do
        log $ styled bold filePath
        for_ lintResults \{ message, sourceRange } ->
          log $ styled red $ "  ✗ " <> (styled (cyan <> underline) $ filePath <> ":" <> show (sourceRange.start.line + 1) <> ":" <> show (sourceRange.start.column + 1)) <> " - " <> message
      else if not hideSuccess then log $ styled (bold <> green) $ "✓︎ " <> filePath
      else pure unit

  , report: \results -> do
      let { yes: successful, no: failed } = Array.partition (Array.null <<< _.lintResults) results
      log ""
      log $ "Successful: " <> (show $ Array.length successful)
      log $ "Failed: " <> (show $ Array.length failed)
  }

type Style = Array GraphicsParam

styled :: Style -> String -> String
styled as str =
  NonEmptyList.fromFoldable as
    # maybe str \as' ->
        escapeCodeToString (AnsiCode.Graphics as')
          <> str
          <> escapeCodeToString (AnsiCode.Graphics $ NonEmptyList.singleton AnsiCode.Reset)

red :: Style
red = [ AnsiCode.PForeground AnsiCode.Red ]

green :: Style
green = [ AnsiCode.PForeground AnsiCode.Green ]

cyan :: Style
cyan = [ AnsiCode.PForeground AnsiCode.Cyan ]

bold :: Style
bold = [ AnsiCode.PMode AnsiCode.Bold ]

underline :: Style
underline = [ AnsiCode.PMode AnsiCode.Underline ]

