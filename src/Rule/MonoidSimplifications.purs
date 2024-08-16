module Rule.MonoidSimplifications where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Newtype as Newtype
import Data.Tuple (Tuple, fst)
import PureScript.CST.Expr as Expr
import PureScript.CST.QualifiedName as QualifiedName
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Expr(..), Ident(..), Operator(..), QualifiedName(..), Wrapped(..))
import Rule (allExpressionsLintProducer)
import Rule as Rule

replaceMaybeMemptyWithFoldMap :: Rule.Rule
replaceMaybeMemptyWithFoldMap = Rule.mkWithNoConfig
  { name: "ReplaceMaybeMemptyWithFoldMap"
  , examples:
      { bad:
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
      , good:
          [ "x = maybe [ 1, 2, 3 ]"
          , "x = maybe \"Hello\""
          , "x = maybe [ 1, 2, 3 ] \\s -> f s 10"
          , "x = maybe \"Hello\" \\s -> f s 10"
          ]
      }
  , lintProducer: allExpressionsLintProducer $ case _ of
      ExprApp (ExprIdent (QualifiedName { token: { range: sourceRange }, name: Ident "maybe" })) appSpines ->
        appSpines
          # NonEmptyArray.head
          # Expr.appTerm
          # foldMap \expr ->
              guard (isMempty expr)
                [ { message: "`maybe` followed by `mempty` can be reduced to `foldMap`", sourceRange } ]
      _ -> []
  }

useGuardOverIfThenElseMEmpty :: Rule.Rule
useGuardOverIfThenElseMEmpty = Rule.mkWithNoConfig
  { name: "UseGuardOverIfThenElseMEmpty"
  , examples:
      { bad:
          [ "x = if 1 == 2 then [ 1, 2, 3 ] else mempty"
          , "x = if 1 == 2 then [ 1, 2, 3 ] else []"
          , "x = if 1 == 2 then \"Hello\" else \"\""
          ]
      , good:
          [ "x = guard (1 == 2) [ 1, 2, 3 ]"
          , "x = guard (1 == 2) \"Hello\""
          , "x = if 1 == 2 then [ 1, 2, 3 ] else [ 4, 5 ]"
          , "x = if 1 == 2 then [ 1, 2, 3 ] else f 10"
          ]
      }
  , lintProducer: allExpressionsLintProducer $ case _ of
      ExprIf { keyword: { range: sourceRange }, false: elseExpr } ->
        guard (isMempty elseExpr)
          [ { message: "Use `guard` when there is a `mempty` in the `else` expression.", sourceRange } ]
      _ -> []
  }

useGuardOverIfThenMemptyElse :: Rule.Rule
useGuardOverIfThenMemptyElse = Rule.mkWithNoConfig
  { name: "UseGuardOverIfThenMEmptyElse"
  , examples:
      { bad:
          [ "x = if 1 == 2 then mempty else [ 1, 2, 3 ]"
          , "x = if 1 == 2 then [] else [ 1, 2, 3 ]"
          , "x = if 1 == 2 then \"\" else \"Hello\""
          ]
      , good:
          [ "x = guard (1 /= 2) [ 1, 2, 3 ]"
          , "x = guard (1 /= 2) \"Hello\""
          , "x = if 1 == 2 then [ 4, 5 ] else [ 1, 2, 3 ]"
          , "x = if 1 == 2 then f 10 else [ 1, 2, 3 ]"
          ]
      }
  , lintProducer: allExpressionsLintProducer $ case _ of
      ExprIf { keyword: { range: sourceRange }, true: thenExpr } ->
        guard (isMempty thenExpr)
          [ { message: "Invert the condition (not) and use `guard` when there is a `mempty` in the `then` expression.", sourceRange } ]
      _ -> []
  }

useFoldForRepeatedMappends :: Rule.Rule
useFoldForRepeatedMappends = Rule.mkWithNoConfig
  { name: "UseFoldForRepeatedMappends"
  , examples:
      { bad:
          [ """
x = 
  (someValues a)
  <> (someDifferentValues b)
  <> (yetOtherValues c)
          """
          ]
      , good:
          [ """
x = 
  (someValues a)
  <> (someDifferentValues b)
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
  , lintProducer: allExpressionsLintProducer $ case _ of
      expr@(ExprOp _ neOperators) ->
        guard (NonEmptyArray.length neOperators > 1 && NonEmptyArray.all isMappend neOperators)
          [ { message: "Use fold When there is more than one <>", sourceRange: rangeOf expr } ]
      _ -> []
  }

isMappend :: forall e. Tuple (QualifiedName Operator) (Expr e) -> Boolean
isMappend = fst >>> QualifiedName.name >>> Newtype.un Operator >>> eq "<>"

isMempty :: forall e11. Expr e11 -> Boolean
isMempty = case _ of
  ExprArray (Wrapped { value: Nothing }) -> true -- []
  ExprString _ "" -> true -- ""
  ExprRecord (Wrapped { value: Nothing }) -> true -- {} (this case seems silly, but hey why not?)
  ExprIdent (QualifiedName { module: Nothing, name: Ident "mempty" }) -> true -- "mempty" (not qualified because assumed it comes from Prelude)
  ExprIdent (QualifiedName { module: Nothing, name: Ident "EQ" }) -> true -- "EQ" (not qualified because assumed it comes from Prelude)
  ExprIdent (QualifiedName { module: Nothing, name: Ident "unit" }) -> true -- "unit" (not qualified because assumed it comes from Prelude)
  _ -> false

