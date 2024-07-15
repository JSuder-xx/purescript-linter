module PureScript.CST.SourceRange where

import Prelude

import Data.Function (on)
import PureScript.CST.Range (class RangeOf, rangeOf)
import PureScript.CST.Types (Expr, Name(..), RecordLabeled(..), SourceRange)

sameLine :: SourceRange -> SourceRange -> Boolean
sameLine = eq `on` _.start.line

spaceBetween :: SourceRange -> SourceRange -> Boolean
spaceBetween first second = first.end.column + 1 == second.start.column

aligned :: SourceRange -> SourceRange -> Boolean
aligned first second = first.start.column == second.start.column

newLineIndent :: SourceRange -> SourceRange -> Boolean
newLineIndent first second = first.end.line < second.start.line && first.start.column < second.start.column

noSpaceBetween :: SourceRange -> SourceRange -> Boolean
noSpaceBetween first second = first.end.column == second.start.column

rangeOfRecordLabeled :: forall e. RangeOf e => RecordLabeled (Expr e) -> SourceRange
rangeOfRecordLabeled (RecordPun (Name { token: { range } })) = range
rangeOfRecordLabeled (RecordField (Name { token: { range: nameRange } }) _ expr) = { start: nameRange.start, end: (rangeOf expr).end }
