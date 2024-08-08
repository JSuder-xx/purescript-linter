module Rule.RecordFormatting (rule) where

import Prelude

import Data.Maybe (Maybe(..))
import Rule (LintResults, expressionLintProducer)
import Rule as Rule
import Rule.Delimited as Delimited
import PureScript.CST.Range (class RangeOf, rangeOf)
import PureScript.CST.SourceRange (newLineIndent, noSpaceBetween, rangeOfRecordLabeled, spaceBetween)
import PureScript.CST.Types (Expr(..), Name(..), RecordLabeled(..))

rule :: Rule.Rule

rule = Rule.mkWithNoConfig
  { name: "RecordFormatting"
  , examples:
      { bad:
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
          ]
      , good:
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
          ]
      }
  , lintProducer: expressionLintProducer $ case _ of
      ExprRecord x -> Delimited.lint config x
      _ -> []
  }
  where
  config =
    { name: "Record"
    , itemName: "Field"
    , openToken: "{"
    , closeToken: "}"
    , innerRange: rangeOfRecordLabeled
    , validateInner: recordLabelIncorrectlySpaced
    }
    where
    recordLabelIncorrectlySpaced :: forall e. RangeOf e => RecordLabeled (Expr e) -> Maybe LintResults
    recordLabelIncorrectlySpaced (RecordPun _) = Nothing
    recordLabelIncorrectlySpaced (RecordField (Name { token: { range: nameRange } }) { range: colonRange } expr) =
      let
        exprRange = rangeOf expr
      in
        if (not $ nameRange `noSpaceBetween` colonRange) then Just [ { message: "Expecting no space between the field label and the `:`", sourceRange: nameRange } ]
        else if (colonRange `spaceBetween` exprRange) || nameRange `newLineIndent` exprRange then Nothing
        else Just [ { message: "Expecting the field expression to either follow the `:` with a single space OR to be on the next line indented", sourceRange: exprRange } ]
