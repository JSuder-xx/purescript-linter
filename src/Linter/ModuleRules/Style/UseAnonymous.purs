module Linter.ModuleRules.Style.UseAnonymous (forOperations, forRecordUpdates, forRecordCreation) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap)
import Data.Map.Extra (keyCountLookup)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Newtype (un, unwrap)
import Data.Tuple (Tuple(..))
import Linter.ModuleRule (RuleCategory(..), expressionIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Binder as Binder
import PureScript.CST.Expr (exprIdent)
import PureScript.CST.Expr as Expr
import PureScript.CST.QualifiedName as QualifiedName
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Separated as Separated
import PureScript.CST.Types (Expr(..), Ident(..), Name(..), QualifiedName, RecordLabeled(..), RecordUpdate(..), Wrapped(..))

forOperations :: ModuleRule.ModuleRule
forOperations = ModuleRule.mkWithNoConfig
  { name: "UseAnonymous-ForOperations"
  , description: "It is easier to read a wildcard operations than a lambda."
  , category: Style
  , examples:
      { failingCode:
          [ "x = \\s -> s < 10"
          , "x = filter (\\s -> s < 10) [ 1, 2, 3 ]"
          ]
      , passingCode:
          [ "x = (_ < 10)"
          , "x = \\s -> M.s < 10"
          , "x = filter (_ < 10) [ 1, 2, 3 ]"
          , "x = \\a' -> guard (a' `op` to) $> a'"
          ]
      }
  , moduleIssueIdentifier: const $ expressionIssueIdentifier $ case _ of
      lambda@(ExprLambda { binders, body }) ->
        binders
          # Binder.lastVariables'
          # foldMap
              ( NonEmptyArray.reverse >>> map unwrap >>> NonEmptyArray.uncons >>> \{ head: firstArgument, tail: restArguments } ->
                  case body of
                    ExprOp leftExpr opExpresions ->
                      case restArguments, NonEmptyArray.toArray opExpresions of
                        [], [ Tuple _ rightExpr ] ->
                          let
                            identifierCount = keyCountLookup $ Expr.allUnqualifiedIdentifiers body
                            leftIdent' = QualifiedName.name <$> Expr.exprIdent leftExpr
                            rightIdent' = QualifiedName.name <$> Expr.exprIdent rightExpr
                          in
                            guard
                              ( (identifierCount firstArgument.name) == 1
                                  && (leftIdent' == Just firstArgument.name || rightIdent' == Just firstArgument.name)
                              )
                              [ { message: "Lambda can be replaced with _.", sourceRange: rangeOf lambda } ]
                        _, _ -> []
                    _ -> []
              )

      _ -> []
  }

forRecordUpdates :: ModuleRule.ModuleRule
forRecordUpdates = ModuleRule.mkWithNoConfig
  { name: "UseAnonymous-ForRecordUpdates"
  , category: Style
  , description: "It is easier to read a wildcard record update than a lambda."
  , examples:
      { failingCode:
          [ "x = \\s -> s { x = 10 }"
          , "x = \\a -> y { a = a }"
          , "x = \\a b -> y { a = a + 1, b = b }"
          , "x = \\a b -> y { a = a, b = b }"
          ]
      , passingCode:
          [ "x = _ { x = 10}"
          , "x = \\a -> _ { x = a }"
          , "x = \\a -> y { a = a + 2 }"
          , "x = \\a -> y { a = f a }"
          , "x = \\a -> y { a = a f }"
          , "x = \\a b -> y { a = a + 2, b = b + 3 }"
          , "x = \\s -> s { x = 10, y = s.y + 1 }"
          , "x = y { a = _ }"
          , "x = y { a = _, b = _ }"
          ]
      }
  , moduleIssueIdentifier: const $ expressionIssueIdentifier $ case _ of
      lambda@(ExprLambda { binders, body }) ->
        binders
          # Binder.lastVariables'
          # foldMap
              ( NonEmptyArray.reverse >>> map unwrap >>> NonEmptyArray.uncons >>> \{ head: firstArgument, tail: restArguments } ->
                  case body of
                    ExprRecordUpdate (ExprSection _) _ -> []
                    ExprRecordUpdate expr (Wrapped { value }) ->
                      let
                        identifierCountInUpdates = keyCountLookup $ QualifiedName.name <$> (recordUpdateToQualifiedIdent =<< Separated.values value)
                        identifierCount = keyCountLookup $ Expr.allUnqualifiedIdentifiers body
                        allArguments = _.name <$> Array.cons firstArgument restArguments
                      in
                        ( Expr.exprIdent expr
                            <#> QualifiedName.name
                            # foldMap \exprIdent ->
                                guard (Array.null restArguments && exprIdent == firstArgument.name && identifierCount exprIdent == 1)
                                  [ { message: "Can be re-written like `_ { a = 1, b = 2 }`", sourceRange: rangeOf lambda } ]
                        )
                          <>
                            ( allArguments
                                # Array.reverse
                                # Array.takeWhile (identifierCountInUpdates >>> eq 1)
                                # Array.uncons
                                # foldMap \{ head, tail } ->
                                    [ { message: "The last arguments to the lambda " <> (Array.intercalate ", " $ (un Ident <$> Array.cons head tail)) <> " occur exactly once in a record update. So it can be re-written with wildcards.", sourceRange: rangeOf lambda } ]
                            )
                    _ -> []
              )

      _ -> []
  }

forRecordCreation :: ModuleRule.ModuleRule
forRecordCreation = ModuleRule.mkWithNoConfig
  { name: "UseAnonymous-ForRecordCreation"
  , description: "It is easier to read a wildcard record creation than to visually tie the arguments to the fields where they are used."
  , category: Style
  , examples:
      { failingCode:

          [ "x = \\a -> { a: a }"
          , "x = \\a b -> { a: a, b: b }"
          , "x = \\a -> { a, b: 10 }"
          , "x = \\a b -> { a: a, b }"
          ]
      , passingCode:
          [ "x = \\a -> { a: a + 10 }"
          , "x = { a: _, b: 10 }"
          , "x = { a: _, b: _ }"
          , "x = \\(Flurp x) -> { b: x, a: _ }"
          , "x = \\test -> { isParallelizable: isAllParallelizable test }"
          , "x = \\test2 -> { isParallelizable: isAllParallelizable test2, test2 }"
          ]

      }
  , moduleIssueIdentifier: const $ expressionIssueIdentifier $ case _ of
      lambda@(ExprLambda { binders, body }) ->
        binders
          # Binder.lastVariables'
          # foldMap
              ( NonEmptyArray.reverse >>> map unwrap >>> NonEmptyArray.uncons >>> \{ head: firstArgument, tail: restArguments } ->
                  case body of
                    ExprRecord (Wrapped { value: Just value }) ->
                      let
                        allArguments = _.name <$> Array.cons firstArgument restArguments
                        identifierCount = keyCountLookup $ Expr.allUnqualifiedIdentifiers body
                        identifierCountInCreate = keyCountLookup $ (recordLabeledToIdent =<< Separated.values value)
                      in
                        allArguments
                          # Array.reverse
                          # Array.takeWhile (\arg -> ((identifierCount arg) <= 1) && (identifierCountInCreate arg) == 1)
                          # Array.uncons
                          # foldMap \{ head, tail } -> [ { message: "The last arguments to the lambda " <> (Array.intercalate ", " $ (un Ident <$> Array.cons head tail)) <> " occur exactly once in a record creation.", sourceRange: rangeOf lambda } ]

                    _ -> []
              )

      _ -> []
  }

recordUpdateToQualifiedIdent :: forall e. RecordUpdate e -> Array (QualifiedName Ident)
recordUpdateToQualifiedIdent = case _ of
  RecordUpdateLeaf _ _ expr -> Expr.exprIdent expr # Array.fromFoldable
  RecordUpdateBranch _ (Wrapped { value }) -> Separated.values value >>= recordUpdateToQualifiedIdent

recordLabeledToIdent :: forall e. RecordLabeled (Expr e) -> Array Ident
recordLabeledToIdent = case _ of
  RecordPun (Name { name }) -> [ name ]
  RecordField _ _ e -> (exprIdent e <#> QualifiedName.name) # Array.fromFoldable
