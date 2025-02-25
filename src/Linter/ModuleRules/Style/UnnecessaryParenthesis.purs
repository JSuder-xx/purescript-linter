module Linter.ModuleRules.Style.UnnecessaryParenthesis (rule) where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Linter.ModuleRule (RuleCategory(..), expressionIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Expr as Expr
import PureScript.CST.Types (Expr(..), Ident(..), Name(..), Operator(..), Proper(..), QualifiedName(..), Type(..), Wrapped(..))

rule :: ModuleRule.ModuleRule
rule = ModuleRule.mkWithNoConfig
  { name: "NoUnnecessaryParenthesis"
  , description:
      """Using parenthesis when unnecessary harms readability with unnecessary noise.

This rule identifies
- Unnecessary parenthesis in type declarations.
- SOME of the cases of unnecessary parenthesis in expressions. It is nowhere near as robust as hlint."""
  , category: Style
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ "x = (1)"
          , "x = (\"Hi\")"
          , "x = (y)"
          , "x = f $ 1"
          , "x = f $ g 23 $ 10"
          , "type X = (Apple)"
          , "type X a = a -> (Apple)"
          , "newtype X a = X (a -> (Apple))"
          , "type X a = a -> (a)"
          , "type X = ({ a :: Int, b :: String })"
          , "type X = Int -> ({ a :: Int, b :: String })"
          , "type X = ({ a :: Int, b :: String }) -> Int"
          , "type X = (Maybe Int) -> String"
          , "type X = Maybe Int -> (F String Int)"
          , "type X = Int -> Int -> Int -> (F String Int)"
          , "type X = Int -> Int -> (F String Int) -> Int"
          , "type X = Int -> Int -> (Int) -> Int"
          , "newtype X a = X (forall f r. a -> (f a) -> r)"
          ]
      , passingCode:
          [ "x = 1"
          , "x = (2 + 3)"
          , "x = (f y)"
          , "x = f $ g 10"
          , "x = (mempty :: Person)"
          , "x = ($)"
          , "x = _ $ 10"
          , "type X = Apple"
          , "type X a = a -> Apple"
          , "newtype X a = X (a -> Apple)"
          , "type X a = a -> a"
          , "type X = Int -> { a :: Int, b :: String }"
          , "type X = { a :: Int, b :: String } -> Int"
          , "type X = Int -> Maybe Int -> Maybe Int -> String"
          , "type X = Maybe Int -> F String Int"
          , "type X = Int -> (Int -> Int) -> String"
          , "newtype X a = X (forall f r. a -> f a -> r)"
          ]
      }
  , moduleIssueIdentifier: const $ expressions { onPureScript { onType = onType } }
  }
  where
  onType = case _ of
    TypeArrow (TypeParens lParens) _ (TypeParens rParens) ->
      parensUnnecessaryInArrow lParens <> parensUnnecessaryInArrow rParens
    TypeArrow (TypeParens lParens) _ _ ->
      parensUnnecessaryInArrow lParens
    TypeArrow _ _ (TypeParens rParens) ->
      parensUnnecessaryInArrow rParens
    TypeParens parens -> terminalParens parens
    _ -> []

  parensUnnecessaryInArrow (Wrapped { open, value }) = case value of
    TypeApp _ _ -> [ { message: "Parentheses are unnecssary around type applications inside the declaration of function type arrows.", sourceRange: open.range } ]
    _ -> []

  terminalParens (Wrapped { open, value }) = case value of
    TypeConstructor (QualifiedName { name: Proper typeConstructor }) -> [ { message: "Unnecessary parenthesis around type constructor `" <> typeConstructor <> "'.", sourceRange: open.range } ]
    TypeVar (Name { name: Ident typeVar }) -> [ { message: "Unnecessary parenthesis around type variable '" <> typeVar <> "'.", sourceRange: open.range } ]
    TypeRecord _ -> [ { message: "Unnecessary parenthesis around a record type.", sourceRange: open.range } ]
    TypeString _ _ -> [ { message: "Unnecessary parenthesis around a type string.", sourceRange: open.range } ]
    TypeInt _ _ _ -> [ { message: "Unnecessary parenthesis around a type int.", sourceRange: open.range } ]
    _ -> []

  expressions = expressionIssueIdentifier $ case _ of
    ExprParens (Wrapped { open, value }) ->
      guard (Expr.isTerminal value) $ pure { message: "Unnecessary parenthesis around a terminal expression.", sourceRange: open.range }
    ExprOp (ExprSection _) _ -> []
    ExprOp _ ne ->
      let
        { last: Tuple (QualifiedName { token, name: (Operator operator) }) expression } = NonEmptyArray.unsnoc ne
      in
        guard (operator == "$" && Expr.isTerminal expression) $ pure { message: "Unnecessary $ preceding a terminal expression.", sourceRange: token.range }
    _ -> []
