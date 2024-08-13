module Rule.ArrayFormatting (rule) where

import Prelude

import Data.Maybe (Maybe(..))
import Rule (allExpressionsLintProducer)
import Rule as Rule
import Rule.Delimited as Delimited
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Expr(..))

rule :: Rule.Rule
rule = Rule.mkWithNoConfig
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
          , """
x = 
  SomeConstructor [ 1
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
  , lintProducer: allExpressionsLintProducer $ case _ of
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

