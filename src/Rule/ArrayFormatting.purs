module Rule.ArrayFormatting (rule) where

import Prelude

import Data.Maybe (Maybe(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Expr(..))
import Rule (expressionIssueIdentifier)
import Rule as Rule
import Rule.Delimited as Delimited

rule :: Rule.Rule
rule = Rule.mkWithNoConfig
  { name: "ArrayFormatting"
  , description: "Ensures consistent spacing when declaring an array literal."
  , examples:
      { failingCode:
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
      , passingCode:
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
  , moduleIssueIdentifier: const $ expressionIssueIdentifier $ case _ of
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
