module Test.Rules where

import Prelude

import Data.Traversable (for_)
import Rule (identifyModuleIssues)
import Rule as Rule
import Rule.AlignedParenthesis as AlignedParenthesis
import Rule.Application as Application
import Rule.ArrayFormatting as ArrayFormatting
import Rule.IfThenElse as IfThenElse
import Rule.LetBinding as LetBinding
import Rule.ModuleExports as ModuleExports
import Rule.MonoidSimplifications as MonoidSimplifications
import Rule.NoDuplicateTypeclassConstraints as NoDuplicateTypeclassConstraints
import Rule.RecordFormatting as RecordFormatting
import Rule.UnnecessarParenthesis as UnnecessarParenthesis
import Rule.UnnecessaryDo as UnnecessaryDo
import Rule.UseAnonymous as UseAnonymous
import Rule.UsePunning as UsePunning
import Rule.WhereClause as WhereClause
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
    describe (Rule.name rule) do
      let
        examples = Rule.examples rule
        config = { indentSpaces: 2 }
      describe "Examples of Failing Code" do
        for_ examples.failingCode \code ->
          it code $ assertCode (f code) \m -> (identifyModuleIssues (Rule.defaultModuleIssueIdentifier rule config) m) `shouldNotEqual` []
      describe "Examples of Passing Code" do
        for_ examples.passingCode \code ->
          it code $ assertCode (f code) \m -> (identifyModuleIssues (Rule.defaultModuleIssueIdentifier rule config) m) `shouldEqual` []
