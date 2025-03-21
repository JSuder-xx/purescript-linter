module Linter.ModuleRules.Formatting.ApplicationIndentation (inArray, inRecord) where

import Prelude

import Data.Array (fold, foldMap)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Linter.ModuleRule (Issue, ModuleRule, RuleCategory(..), expressionIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Separated as Separated
import PureScript.CST.Types (AppSpine, Expr(..), Name(..), RecordLabeled(..), Wrapped(..))

inArray :: ModuleRule
inArray = ModuleRule.mkWithNoConfig
  { name: "Application.InArray.IndentArguments"
  , category: Formatting
  , description:
      """This very limited rule ensures proper function call argument indentation when the function call is made inside an array.

While limited, this rule helps with the formatting of function calls in render functions."""
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ """
x =
  [ div
    []
    []
  ]
          """
          , """
x =
  [ span
   [] []
  ]
          """
          , """
x =
  [ div
     [] []
  ]
          """
          ]
      , passingCode:
          [ """
x =
  [ div [] [] ]
          """
          , """
x =
  [ div []
      []
  ]
          """
          , """
x =
  [ div
      [] []
  ]
          """
          , """
x =
  [ div
      []
      []
  ]
          """
          , """
x =
  [ div [] []
  , div
      []
      []
  ]
          """
          ]
      }
  , moduleIssueIdentifier: \{ indentSpaces } -> expressionIssueIdentifier $
      case _ of
        ExprArray (Wrapped { value: Just separatedExprs }) ->
          Separated.values separatedExprs
            # foldMap
                case _ of
                  ExprApp fExpr spines ->
                    let
                      functionRange = rangeOf fExpr
                    in
                      checkSpines
                        { context: "applied from inside an array"
                        , indentOffColumn: functionRange.start.column
                        , indentOffLabel: "function"
                        , indentSpaces
                        , initialLine: functionRange.start.line
                        }
                        spines
                  _ -> []

        _ -> []

  }

inRecord :: ModuleRule
inRecord = ModuleRule.mkWithNoConfig
  { name: "Application.InRecord.IndentArguments"
  , category: Formatting
  , description: "This rule has a very limited scope: It ensures proper function call argument indentation when the function call is made while declaring a record literal."
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ """
x =
  { foo: f
         1
         2
  }
          """
          , """
x =
  { foo: f
           1
  }
          """
          , """
x =
  { foo:
    f
    1
  }
          """
          , """
x =
  if b then
    { foo:
      f
      1
    }
  else
    { foo: 1 }
          """
          ]
      , passingCode:
          [ """
x =
  { foo: f 1 }
          """
          , """
x =
  { foo: f 1
      3
  }
          """
          , """
x =
  { foo: f
      3 4
  }
          """
          , """
x =
  { foo: f
      1
      2
  }
          """
          , """
x =
  { foo: g 1 2
  , bar: f
      1
      2
  }
          """
          , """
x =
  { foo: g 1 2
  , bar:
      f
        1
        2
  }
          """
          , """
x =
  if b then
    { foo:
        f
          1
    }
  else
    { foo:
        f 1
    }
          """

          ]
      }
  , moduleIssueIdentifier: \{ indentSpaces } -> expressionIssueIdentifier $ case _ of
      ExprRecord (Wrapped { value: Just separatedExprs }) ->
        Separated.values separatedExprs
          # foldMap
              case _ of
                RecordField (Name { token: { range: fieldLabelRange } }) _ (ExprApp fExpr spines) ->
                  let
                    functionRange = rangeOf fExpr
                    { indentOffLabel, indentOffColumn } =
                      -- When the fieldLabel and functionExpr are on the same line then indent off the field label
                      -- fieldLabel: functionExpr
                      if fieldLabelRange.start.line == functionRange.start.line then
                        { indentOffLabel: "field label", indentOffColumn: fieldLabelRange.start.column }
                      -- otherwise indent off the function AND the function must be indented
                      else
                        { indentOffLabel: "function expression", indentOffColumn: functionRange.start.column }
                  in
                    checkSpines
                      { initialLine: functionRange.start.line
                      , context: "when declaring a record field literal"
                      , indentSpaces
                      , indentOffColumn
                      , indentOffLabel
                      }
                      spines
                _ -> []

      _ -> []

  }

checkSpines
  :: forall f
   . Foldable f
  => { context :: String
     , indentOffColumn :: Int
     , indentOffLabel :: String
     , indentSpaces :: Int
     , initialLine :: Int
     }
  -> f (AppSpine Expr Void)
  -> Array Issue
checkSpines { initialLine, indentSpaces, indentOffColumn, indentOffLabel, context } =
  _.errors <<<
    foldl
      ( \acc spine ->
          let
            spineRange = rangeOf spine
          in
            { lastLine: spineRange.start.line
            , lastIsFunction: false
            , errors: acc.errors
                <> guard (not ((acc.lastLine == spineRange.start.line) || (spineRange.start.column - indentSpaces == indentOffColumn)))
                  [ { message:
                        if acc.lastIsFunction then
                          fold [ "The first argument to a function (", context, ") must either be on the same line as the function or on the next line indented off the ", indentOffLabel ]
                        else
                          fold [ "Every argument to a function (", context, ") must either be on the same line as the preceding argument OR it must be on the next line indented off the ", indentOffLabel ]
                    , sourceRange: spineRange
                    }
                  ]
            }
      )
      { errors: [], lastLine: initialLine, lastIsFunction: true }
