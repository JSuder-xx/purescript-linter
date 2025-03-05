module Linter.ModuleRules.Style.AvoidSameComparison where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Linter.ModuleRule (RuleCategory(..), expressionIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Equivalent (equivalent)
import PureScript.CST.Expr as Expr
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Operator(..), QualifiedName(..))

avoidSameComparison :: ModuleRule.ModuleRule
avoidSameComparison = ModuleRule.mkWithNoConfig
  { name: "AvoidSameComparison"
  , category: Style
  , description:
      """This rule forbids comparing two equivalent expressions because this will always return either true or false.
- `a == a`
- `a /= a`
- `a > a`
- `a >= a`
- etc.."""
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ "x a = a == a"
          , "x a = a /= a"
          , "x a = a > a"
          , "x a = a >= a"
          , "x a = a < a"
          , "x a = a <= a"
          ]
      , passingCode:
          [ "x a = a == 1"
          , "x a = a <> a -- irrelevant operator"
          , "x a = a == b"
          , "x a = a /= 1"
          , "x a = a > (a + 1)"
          ]
      }
  , moduleIssueIdentifier: \_ -> expressionIssueIdentifier $ Expr.binaryOperation
      >>> foldMap
        ( \{ leftExpr, rightExpr, qualifiedOperatorName: QualifiedName { name: Operator operatorName } } ->
            if not Array.elem operatorName operatorNames then []
            else if not equivalent leftExpr rightExpr then []
            else
              [ { message: "Binary operation '" <> operatorName <> "' has the same exact expression on right as the left so will always return either True or False.", sourceRange: rangeOf rightExpr } ]
        )
  }
  where
  operatorNames = [ "==", "/=", "<", "<=", ">", ">=" ]
