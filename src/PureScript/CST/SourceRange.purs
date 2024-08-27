module PureScript.CST.SourceRange where

import Prelude

import Data.Function (on)
import PureScript.CST.Range (class RangeOf, rangeOf)
import PureScript.CST.Types (Expr, Name(..), RecordLabeled(..), SourceRange)

sameLine :: SourceRange -> SourceRange -> Boolean
sameLine = eq `on` _.start.line

spaceBetween :: SourceRange -> SourceRange -> Boolean
spaceBetween first second = first.start.line == second.start.line && first.end.column + 1 == second.start.column

leftAligned :: SourceRange -> SourceRange -> Boolean
leftAligned first second = first.start.column == second.start.column

isAbove :: SourceRange -> SourceRange -> Boolean
isAbove first second = first.end.line < second.start.line

newLineIndent :: Int -> SourceRange -> SourceRange -> Boolean
newLineIndent indent first second = first.end.line < second.start.line && first.start.column + indent == second.start.column

noSpaceBetween :: SourceRange -> SourceRange -> Boolean
noSpaceBetween first second = first.end.column == second.start.column

rangeOfRecordLabeled :: forall e. RangeOf e => RecordLabeled (Expr e) -> SourceRange
rangeOfRecordLabeled (RecordPun (Name { token: { range } })) = range
rangeOfRecordLabeled (RecordField (Name { token: { range: nameRange } }) _ expr) = { start: nameRange.start, end: (rangeOf expr).end }
