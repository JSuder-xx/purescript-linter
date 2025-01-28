module Rule.RecordFormatting (rule) where

import Prelude

import Data.Maybe (Maybe(..))
import PureScript.CST.Range (class RangeOf, rangeOf)
import PureScript.CST.SourceRange (noSpaceBetween, rangeOfRecordLabeled, spaceBetween)
import PureScript.CST.Types (Expr(..), Name(..), RecordLabeled(..))
import Rule (Issue, expressionIssueIdentifier)
import Rule as Rule
import Rule.Delimited as Delimited

rule :: Rule.Rule
rule = Rule.mkWithNoConfig
  { name: "RecordFormatting"
  , description:
      "Ensures consistent spacing when declaring a record literal."
  , examples:
      { failingCode
      , passingCode
      }
  , moduleIssueIdentifier: \{ indentSpaces } -> expressionIssueIdentifier $ case _ of
      ExprRecord x -> Delimited.lint (config indentSpaces) x
      _ -> []
  }
  where
  config identSpaces =
    { name: "Record"
    , itemName: "Field"
    , openToken: "{"
    , closeToken: "}"
    , innerRange: rangeOfRecordLabeled
    , validateInner: recordLabelIncorrectlySpaced
    }
    where
    recordLabelIncorrectlySpaced :: forall e. RangeOf e => RecordLabeled (Expr e) -> Maybe (Array Issue)
    recordLabelIncorrectlySpaced (RecordPun _) = Nothing
    recordLabelIncorrectlySpaced (RecordField (Name { token: { range: nameRange } }) { range: colonRange } expr) =
      let
        exprRange = rangeOf expr
      in
        if (not $ nameRange `noSpaceBetween` colonRange) then Just [ { message: "Expecting no space between the field label and the `:`", sourceRange: nameRange } ]
        else if (colonRange `spaceBetween` exprRange) || ((nameRange.start.line < exprRange.start.line) && ((nameRange.start.column + identSpaces) == exprRange.start.column)) then Nothing
        else Just [ { message: "Expecting the field expression to either follow the `:` with a single space OR to be on the next line with the value aligned with the field label", sourceRange: exprRange } ]

failingCode :: Array String
failingCode =
  [ "x = { }"
  , "x = {   }"
  , "x = {x: 1}"
  , "x = { x: 1}"
  , "x = {x: 1 }"
  , "x = { x :1 }"
  , "x = { x:1 }"
  , "x = { x: 1, y: 1}"
  , "x = { x: 1, y:1 }"
  , "x = { x:1, y: 1 }"
  , "x = { x: 1,y: 1 }"
  , "x = { x: 1, y: 1, z:3 }"
  , "x = { x: 1, y: 1,z: 3 }"
  , """
x =
  { a: 1
  , b: 2 }
          """
  , """
x =
  { a: 1
  ,b: 2
  }
          """
  , """
x =
  { a: 1
    ,b: 2
  }
          """
  , """
x =
  SomeConstructor
    { a: 1
    ,b: 2
    }
"""
  , """
x =
  f
    { a: 1
    ,b: 2
    }
"""
  , """
x =
  { a:
  1
  , b: 2
  }
          """
  , """
x =
  { a:
   1
  , b: 2
  }
          """
  , """
x =
  { a:
    1
  , b: 2
  }
          """
  , """
x =
  { a:
     1
  , b: 2
  }
          """
  , """
x =
  { a:
       1
  , b: 2
  }
          """
  ]

passingCode :: Array String
passingCode =
  [ "x = {}"
  , "x = { x: 1 }"
  , "x = { y }"
  , "x = { x: 1, y: 2 }"
  , "x = { x: 1, y: 2, z: 3 }"
  , """
x =
  { a: 1
  }
      """
  , """
x =
  { a: 1
  , b: 2
  }
      """
  , """
x =
  { a:
      1
  , b: 2
  }
          """
  ]
