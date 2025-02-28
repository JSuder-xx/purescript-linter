module Linter.ModuleRules.Style.ApplicativeSimplifications where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap)
import Linter.ModuleRule (RuleCategory(..), expressionIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Expr (appTerm)
import PureScript.CST.Types (Expr(..), Ident(..), QualifiedName(..), Wrapped(..))

unlessAndWhen :: ModuleRule.ModuleRule
unlessAndWhen = ModuleRule.mkWithNoConfig
  { name: "Applicative.UnlessAndWhen"
  , category: Style
  , description:
      """This rule asks that developers replace
- `when (not EXPR)` with `unless EXPR`
- `unless (not EXPR)` with `when EXPR`

In order to remove negation. Human beings read positive statements more easily."""
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ "x a = when (not a) something"
          , "x a b = when (not $ a || b) something"
          , "x a b = when (not (a || b)) something"
          , "x a = unless (not a) something"
          ]
      , passingCode:
          [ "x a = when a something"
          , "x a = unless a something"
          ]
      }
  , moduleIssueIdentifier: const $ expressionIssueIdentifier $ case _ of
      ExprApp (ExprIdent (QualifiedName { token: { range }, name: Ident functionName })) appSpinesNE ->
        let
          checkFlip fn flippedFn =
            if functionName /= fn then []
            else
              appSpinesNE
                # NonEmptyArray.head
                # appTerm
                # foldMap
                    ( \expr ->
                        if isNotCall expr then [ { message: "Use `" <> flippedFn <> "` to avoid an unncessary NOT.", sourceRange: range } ]
                        else []
                    )
        in
          (checkFlip "when" "unless") <> (checkFlip "unless" "when")
      _ -> []
  }
  where
  isNotCall (ExprOp op _) = isNotIdent op
  isNotCall (ExprApp fnIdent _) = isNotIdent fnIdent
  isNotCall (ExprParens (Wrapped { value: inner })) = isNotCall inner
  isNotCall _ = false

  isNotIdent (ExprIdent (QualifiedName { name: Ident "not" })) = true
  isNotIdent (ExprParens (Wrapped { value: inner })) = isNotIdent inner
  isNotIdent _ = false
