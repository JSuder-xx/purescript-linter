module Test.Rules where

import Prelude

import Data.Traversable (for_)
import Linter.ModuleRule (identifyModuleIssues)
import Linter.ModuleRule as ModuleRule
import Linter.ModuleRules.AlignedParenthesis as AlignedParenthesis
import Linter.ModuleRules.Application as Application
import Linter.ModuleRules.ArrayFormatting as ArrayFormatting
import Linter.ModuleRules.IfThenElse as IfThenElse
import Linter.ModuleRules.LetBinding as LetBinding
import Linter.ModuleRules.ModuleExports as ModuleExports
import Linter.ModuleRules.MonoidSimplifications as MonoidSimplifications
import Linter.ModuleRules.NoDuplicateTypeclassConstraints as NoDuplicateTypeclassConstraints
import Linter.ModuleRules.RecordFormatting as RecordFormatting
import Linter.ModuleRules.UnnecessarParenthesis as UnnecessarParenthesis
import Linter.ModuleRules.UnnecessaryDo as UnnecessaryDo
import Linter.ModuleRules.UseAnonymous as UseAnonymous
import Linter.ModuleRules.UsePunning as UsePunning
import Linter.ModuleRules.WhereClause as WhereClause
import Test.Common (assertCode, simpleModulePrefix)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

main :: Spec Unit
main = describe "Linters" do
  for_
    [ Application.inArray
    , Application.inRecord
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
    , UnnecessarParenthesis.rule
    , UseAnonymous.forOperations
    , UseAnonymous.forRecordUpdates
    , UseAnonymous.forRecordCreation
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
