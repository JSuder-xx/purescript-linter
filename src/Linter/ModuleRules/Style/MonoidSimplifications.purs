module Linter.ModuleRules.Style.MonoidSimplifications where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Monoid (guard)
import Data.Newtype as Newtype
import Data.Tuple (Tuple, fst, snd)
import Linter.ModuleRule (RuleCategory(..), expressionIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Expr as Expr
import PureScript.CST.QualifiedName as QualifiedName
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Expr(..), Ident(..), Operator(..), QualifiedName(..), Wrapped(..))

replaceMaybeMemptyWithFoldMap :: ModuleRule.ModuleRule
replaceMaybeMemptyWithFoldMap = ModuleRule.mkWithNoConfig
  { name: "ReplaceMaybeMemptyWithFoldMap"
  , category: Style
  , description:
      """
Replacing `maybe mempty` with `foldMap` is a bit more succinct and more clearly expresses the intention.
  """
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ "x = maybe mempty"
          , "x = maybe \"\""
          , "x = maybe []"
          , "x = maybe mempty show"
          , "x = maybe \"\" show"
          , "x = maybe [] pure"
          , "x = maybe mempty \\s -> f s 10"
          , "x = maybe \"\" \\s -> f s 10"
          , "x = maybe [] \\s -> f s 10"
          ]
      , passingCode:
          [ "x = maybe [ 1, 2, 3 ]"
          , "x = maybe \"Hello\""
          , "x = maybe [ 1, 2, 3 ] \\s -> f s 10"
          , "x = maybe \"Hello\" \\s -> f s 10"
          ]
      }
  , moduleIssueIdentifier: const $ expressionIssueIdentifier $ case _ of
      ExprApp (ExprIdent (QualifiedName { token: { range: sourceRange }, name: Ident "maybe" })) appSpines ->
        appSpines
          # NonEmptyArray.head
          # Expr.appTerm
          # foldMap \expr ->
              guard (isMempty expr)
                [ { message: "`maybe` followed by `mempty` can be reduced to `foldMap`", sourceRange } ]
      _ -> []
  }

useGuardOverIfThenElseMEmpty :: ModuleRule.ModuleRule
useGuardOverIfThenElseMEmpty = ModuleRule.mkWithNoConfig
  { name: "UseGuardOverIfThenElseMEmpty"
  , category: Style
  , description:
      """Replacing `if EXPR then TRUE else mempty` with `guard EXPR TRUE` reduces cognitive overhead because the reader should not be \"interested\" in the false branch.

This rule ONLY applies when the TRUE branch is considered a quickly executing expression. Expressions are considered quickly executing if they do not make function calls.
The reason expressions must be quickly executing is that the TRUE branch always evaluates when using `guard` and if that is slow you are better off using `if/then/else` which
delays execution until necessary.

This is similar to using `when EXPR TRUE` rather than `if EXPR then TRUE else pure unit` when working the applicatives.
"""
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ "x = if 1 == 2 then [ 1, 2, 3 ] else mempty"
          , "x = if 1 == 2 then [ 1, 2, 3 ] else []"
          , "x = if 1 == 2 then \"Hello\" else \"\""
          , "x = if 1 == 2 then [ { x: 10 } ] else []"
          , "x = if 1 == 2 then [ { x: [ 10 ] } ] else []"
          , "x = if 1 == 2 then [ { x: Apples 10 } ] else []"
          , "x = if 1 == 2 then f <> g else []"
          ]
      , passingCode:
          [ "x = guard (1 == 2) [ 1, 2, 3 ]"
          , "x = guard (1 == 2) \"Hello\""
          , "x = if 1 == 2 then [ 1, 2, 3 ] else [ 4, 5 ]"
          , "x = if 1 == 2 then [ 1, 2, 3 ] else f 10"
          , "x = if 1 == 2 then f 10 else []"
          , "x = if 1 == 2 then [ { x: Apples $ f 10 } ] else [] -- function call to `f` makes this slow so guard is not recommended"
          , "x = if 1 == 2 then [ { x: Apples 10 $ f 10 } ] else [] -- function call to `f` makes this slow so guard is not recommended"
          , "x = if 1 == 2 then f <> g 10 else [] -- function call to `g` makes this slow so guard is not recommended"
          ]
      }
  , moduleIssueIdentifier: const $ expressionIssueIdentifier $ case _ of
      ExprIf { keyword: { range: sourceRange }, true: trueExpr, false: elseExpr } ->
        guard ((isMempty elseExpr) && Expr.isFast trueExpr)
          [ { message: "Use `guard` when there is a `mempty` in the `else` expression.", sourceRange } ]
      _ -> []
  }

useGuardOverIfThenMemptyElse :: ModuleRule.ModuleRule
useGuardOverIfThenMemptyElse = ModuleRule.mkWithNoConfig
  { name: "UseGuardOverIfThenMEmptyElse"
  , category: Style
  , description:
      """Replacing `if EXPR then mempty else FALSE` with `guard (not EXPR) FALSE` _may_ reduce cognitive overhead. However, this case is a little more controversial.

This rule ONLY applies when the FALSE branch is considered a "quickly executing expression". Expressions are considered quickly executing if they do not make function calls.
The reason expressions must be quickly executing is that the expression always evaluates when using `guard` and if that is slow you are better off using `if/then/else` which
delays execution until necessary.
      """
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ "x = if 1 == 2 then mempty else [ 1, 2, 3 ]"
          , "x = if 1 == 2 then [] else [ 1, 2, 3 ]"
          , "x = if 1 == 2 then \"\" else \"Hello\""
          ]
      , passingCode:
          [ "x = guard (1 /= 2) [ 1, 2, 3 ]"
          , "x = guard (1 /= 2) \"Hello\""
          , "x = if 1 == 2 then [ 4, 5 ] else [ 1, 2, 3 ]"
          , "x = if 1 == 2 then f 10 else [ 1, 2, 3 ]"
          ]
      }
  , moduleIssueIdentifier: const $ expressionIssueIdentifier $ case _ of
      ExprIf { keyword: { range: sourceRange }, false: falseExpr, true: trueExpr } ->
        guard ((isMempty trueExpr) && Expr.isFast falseExpr)
          [ { message: "Invert the condition (not) and use `guard` when there is a `mempty` in the `then` expression.", sourceRange } ]
      _ -> []
  }

useFoldForRepeatedMappends :: ModuleRule.ModuleRule
useFoldForRepeatedMappends = ModuleRule.mkWithNoConfig
  { name: "UseFoldForRepeatedMappends"
  , category: Style
  , description:
      """
Using `fold`
1. Removes the need for parenthesis which reduces lexical noise.
2. Is more succinct, in terms of characters, when there are 8 or more mappends.
  """
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ """
x =
  (someValues a)
  <> (someDifferentValues b)
  <> (yetOtherValues c)
  <> (more d)
          """
          , """
x =
  "Hi!"
  <> a
  <> b
  <> c
  <> d
  <> e
          """
          ]
      , passingCode:
          [ """
x =
  (someValues a)
  <> (someDifferentValues b)
          """
          , """
x =
  "first "
  <> f "second"
  <> "third"
          """
          , """
x =
  fold
    [ someValues a
    , someDifferentValues b
    , yetOtherValues c
    ]
          """
          ]
      }
  , moduleIssueIdentifier: const $ expressionIssueIdentifier $ case _ of
      expr@(ExprOp _ neOperators) ->
        guard (NonEmptyArray.all isMappend neOperators)
          let
            numOperators = NonEmptyArray.length neOperators
          in
            if (numOperators >= 5) then
              [ { message: "Use fold when there are five or more <> operators", sourceRange: rangeOf expr } ]
            else guard (numOperators >= 3 && NonEmptyArray.any hasParenthesis neOperators)
              [ { message: "Use fold when there are three or more <> operators and any expressions require parenthesis ", sourceRange: rangeOf expr } ]
      _ -> []
  }

isMappend :: forall e. Tuple (QualifiedName Operator) (Expr e) -> Boolean
isMappend = fst >>> QualifiedName.name >>> Newtype.un Operator >>> eq "<>"

hasParenthesis :: forall t114 t121. Tuple t114 (Expr t121) -> Boolean
hasParenthesis = snd >>> Expr.exprParens >>> Maybe.isJust

isMempty :: forall e11. Expr e11 -> Boolean
isMempty = case _ of
  ExprArray (Wrapped { value: Nothing }) -> true -- []
  ExprString _ "" -> true -- ""
  ExprRecord (Wrapped { value: Nothing }) -> true -- {} (this case seems silly, but hey why not?)
  ExprIdent (QualifiedName { module: Nothing, name: Ident "mempty" }) -> true -- "mempty" (not qualified because assumed it comes from Prelude)
  ExprIdent (QualifiedName { module: Nothing, name: Ident "EQ" }) -> true -- "EQ" (not qualified because assumed it comes from Prelude)
  ExprIdent (QualifiedName { module: Nothing, name: Ident "unit" }) -> true -- "unit" (not qualified because assumed it comes from Prelude)
  _ -> false
