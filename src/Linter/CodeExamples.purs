module Linter.CodeExamples where

import Data.Foldable (fold)
import Data.String as String

exampleWithExports :: { exports :: Array String } -> String -> String
exampleWithExports { exports } body = fold
  [ "module Test ("
  , String.joinWith ", " exports
  , ") where\n"
  , body
  ]
