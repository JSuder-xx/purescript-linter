module PureScript.CST.SourcePos where

import Prelude

import Data.Function (on)
import PureScript.CST.Types (SourcePos)

sameLine :: SourcePos -> SourcePos -> Boolean
sameLine = eq `on` _.line

columnDifference :: SourcePos -> SourcePos -> Int
columnDifference = sub `on` _.column
