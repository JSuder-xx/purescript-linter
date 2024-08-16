module Rule.Delimited where

import Prelude

import Data.Array (fold)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Tuple (Tuple(..), snd)
import PureScript.CST.SourcePos (columnDifference)
import PureScript.CST.SourceRange (leftAligned, noSpaceBetween, spaceBetween)
import PureScript.CST.Types (Delimited, Separated(..), SourceRange, Wrapped(..), SourceToken)
import Rule (LintResults)

lint :: forall inner. { name :: String, itemName :: String, openToken :: String, closeToken :: String, innerRange :: inner -> SourceRange, validateInner :: inner -> Maybe LintResults } -> Delimited inner -> LintResults
lint { name, openToken, closeToken } (Wrapped { open: { range: openRange }, close: { range: closeRange }, value: Nothing }) =
  guard ((closeRange.start `columnDifference` openRange.start) > 1) $ pure { message: "An empty " <> name <> " should be " <> openToken <> closeToken, sourceRange: openRange }
lint { name, itemName, openToken, closeToken, innerRange, validateInner } (Wrapped { open: { range: openRange }, close: { range: closeRange }, value: Just (Separated { head, tail }) }) =
  if openRange.start.line == closeRange.start.line then
    checkSingleLine unit
  else
    checkMultiLine unit

  where
  checkSingleLine :: Unit -> LintResults
  checkSingleLine _ =
    case Array.unsnoc tail of
      Nothing ->
        guard (not $ (openRange `spaceBetween` rangeHead) && (rangeHead `spaceBetween` closeRange)) [ { message: "Missing space between surrounding " <> openToken <> closeToken <> " and contents.", sourceRange: openRange } ]
          <> (validateInner head # fromMaybe [])
      Just { last: Tuple _ lastExpr } ->
        let
          rangeLast = innerRange lastExpr
          allLabeledRecords = Array.cons head $ snd <$> tail
        in
          fold
            [ guard (not $ (openRange `spaceBetween` rangeHead))
                [ { message: "Missing space between " <> openToken <> " and " <> itemName, sourceRange: openRange } ]
            , guard (not (rangeLast `spaceBetween` closeRange)) [ { message: "Missing space between last " <> itemName <> " and " <> closeToken, sourceRange: closeRange } ]
            , (Array.findMap validateInner allLabeledRecords # fromMaybe [])
            , let
                step :: { failed :: Array SourceRange, lastExprRange :: SourceRange } -> Tuple SourceToken inner -> { failed :: Array SourceRange, lastExprRange :: SourceRange }
                step { failed, lastExprRange } (Tuple { range: delimitterRange } expr) =
                  let
                    thisExprRange = innerRange expr
                  in
                    { failed:
                        if (lastExprRange `noSpaceBetween` delimitterRange) && (delimitterRange `spaceBetween` thisExprRange) then
                          failed
                        else
                          Array.snoc failed lastExprRange
                    , lastExprRange: thisExprRange
                    }
              in
                (Array.foldl step { failed: [], lastExprRange: rangeHead } tail).failed <#> { message: "Incorrect spacing between the comma and preceding " <> itemName, sourceRange: _ }
            ]

    where
    rangeHead = innerRange head

  checkMultiLine :: Unit -> LintResults
  checkMultiLine _ =
    case Array.unsnoc tail of
      Nothing ->
        guard (not $ (openRange `spaceBetween` rangeHead)) [ { message: "Missing space between " <> openToken <> " and the first " <> itemName, sourceRange: rangeHead } ]
          <> guard (not $ (openRange `leftAligned` closeRange)) [ { message: "The closing " <> closeToken <> " should align with the opening " <> openToken, sourceRange: closeRange } ]
          <> (validateInner head # fromMaybe [])
      Just { last: Tuple _ lastExpr } ->
        let
          rangeLast = innerRange lastExpr
          allLabeledRecords = Array.cons head $ snd <$> tail
        in
          fold
            [ guard (not $ (openRange `spaceBetween` rangeHead))
                [ { message: "Missing space between { first field.", sourceRange: openRange } ]
            , guard (rangeLast.start.line >= closeRange.start.line || closeRange.start.column /= openRange.start.column)
                [ { message: "Closing } should be on a new line", sourceRange: closeRange } ]
            , (Array.findMap validateInner allLabeledRecords # fromMaybe [])
            , ( tail >>= \(Tuple { range } expr) ->
                  guard (range.start.column /= openRange.start.column) [ { message: "With multiline " <> name <> ", the ',' must align with the " <> openToken, sourceRange: range } ]
                    <>
                      ( let
                          exprRange = innerRange expr
                        in
                          guard (not $ range `spaceBetween` exprRange) [ { message: "With multiline " <> name <> ", there should be a space between the leading `,` and the " <> itemName, sourceRange: exprRange } ]
                      )
              )
            ]
    where
    rangeHead = innerRange head

