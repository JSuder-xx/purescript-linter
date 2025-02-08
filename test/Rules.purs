module Test.Rules where

import Prelude

import Data.Traversable (for_)
import Linter.ModuleRule (identifyModuleIssues)
import Linter.ModuleRule as ModuleRule
import Linter.ModuleRules.Formatting.AlignedParenthesis as AlignedParenthesis
import Linter.ModuleRules.Formatting.ApplicationIndentation as ApplicationIndentation
import Linter.ModuleRules.Formatting.ArrayFormatting as ArrayFormatting
import Linter.ModuleRules.Formatting.IfThenElse as IfThenElse
import Linter.ModuleRules.Formatting.LetBinding as LetBinding
import Linter.ModuleRules.Formatting.RecordFormatting as RecordFormatting
import Linter.ModuleRules.Formatting.WhereClause as WhereClause
import Linter.ModuleRules.Style.ModuleExports as ModuleExports
import Linter.ModuleRules.Style.MonoidSimplifications as MonoidSimplifications
import Linter.ModuleRules.Style.NoDuplicateTypeclassConstraints as NoDuplicateTypeclassConstraints
import Linter.ModuleRules.Style.UnnecessaryDo as UnnecessaryDo
import Linter.ModuleRules.Style.UnnecessaryParenthesis as UnnecessaryParenthesis
import Linter.ModuleRules.Style.UseAnonymous as UseAnonymous
import Linter.ModuleRules.Style.UseForallSymbol as UseForallSymbol
import Linter.ModuleRules.Style.UsePunning as UsePunning
import Test.Common (assertCode, simpleModulePrefix)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

main :: Spec Unit
main = describe "Linters" do
  for_
    [ ApplicationIndentation.inArray
    , ApplicationIndentation.inRecord
    , AlignedParenthesis.rule
    , ArrayFormatting.rule
    , IfThenElse.ifThenElseLeftAligned
    , LetBinding.compact
    , MonoidSimplifications.replaceMaybeMemptyWithFoldMap
    , MonoidSimplifications.useFoldForRepeatedMappends
    , MonoidSimplifications.useGuardOverIfThenElseMEmpty
    , MonoidSimplifications.useGuardOverIfThenMemptyElse
    , NoDuplicateTypeclassConstraints.rule
    , RecordFormatting.rule
    , UnnecessaryDo.rule
    , UnnecessaryParenthesis.rule
    , UseAnonymous.forOperations
    , UseAnonymous.forRecordUpdates
    , UseAnonymous.forRecordCreation
    , UseForallSymbol.rule
    , UsePunning.rule
    , WhereClause.whereLeftAligned
    ]
    $ testRuleWithCode (simpleModulePrefix <> _)

  for_ [ ModuleExports.exportsRequired ]
    $ testRuleWithCode identity
  where
  testRuleWithCode f rule =
    describe (ModuleRule.name rule) do
      let
        examples = ModuleRule.examples rule
        config = { indentSpaces: 2 }
      describe "Examples of Failing Code" do
        for_ examples.failingCode \code ->
          it code $ assertCode (f code) \m -> (identifyModuleIssues (ModuleRule.defaultModuleIssueIdentifier rule config) m) `shouldNotEqual` []
      describe "Examples of Passing Code" do
        for_ examples.passingCode \code ->
          it code $ assertCode (f code) \m -> (identifyModuleIssues (ModuleRule.defaultModuleIssueIdentifier rule config) m) `shouldEqual` []
