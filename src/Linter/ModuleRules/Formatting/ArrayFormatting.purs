module Linter.ModuleRules.Formatting.ArrayFormatting (rule) where

import Prelude

import Data.Maybe (Maybe(..))
import Linter.ModuleRule (RuleCategory(..), expressionIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import Linter.ModuleRules.Formatting.Delimited as Delimited
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Expr(..))

rule :: ModuleRule.ModuleRule
rule = ModuleRule.mkWithNoConfig
  { name: "ArrayFormatting"
  , category: Formatting
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
