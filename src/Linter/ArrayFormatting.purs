module Linter.ArrayFormatting (linter) where

import Prelude

import Data.Maybe (Maybe(..))
import Linter (expressionLintProducer)
import Linter as Linter
import Linter.Delimited as Delimited
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Expr(..))

linter :: Linter.Linter
linter =
  { name: "ArrayFormatting"
  , examples:
      { bad:
          [ "x = [ ]"
          , "x = [    ]"
          , "x = [1]"
          , "x = [1, 2]"
          , "x = [ 1,2 ]"
          , "x = [ 1, 2 , 3 ]"
          , """
x =
  [1
  ]
"""
          , """
x =
  [ 1
   ]
"""
          , """
x = 
  [ 1
  ,2 
  ,3
  ]
"""
          , """
x = 
  [1
  , 2 
  , 3
  ]
"""
          , """
x = 
  [ 1
  , 2, 3 ]
"""

          ]
      , good:
          [ "x = []"
          , "x = [ [] ]"
          , "x = [ 1 ]"
          , "x = [ 1, 2 ]"
          , "x = [ 1, 2, 3 ]"
          , """
x =
  [ 1
  ]
"""
          , """
x = 
  [ 1
  , 2 
  , 3
  ]
"""
          ]
      }
  , lintProducer: expressionLintProducer $ case _ of
      ExprArray x -> Delimited.lint config x
      _ -> []
  }
  where
  config =
    { name: "Array"
    , itemName: "Element"
    , openToken: "["
    , closeToken: "]"
    , innerRange: rangeOf
    , validateInner: const Nothing
    }

