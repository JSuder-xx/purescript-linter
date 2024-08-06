module Test.Linter where

import Prelude

import Data.Traversable (for_)
import Linter (runLintProducer)
import Linter as Linter
import Linter.AlignedParenthesis as AlignedParenthesis
import Linter.ArrayFormatting as ArrayFormatting
import Linter.IfThenElse as IfThenElse
import Linter.LetBinding as LetBinding
import Linter.ModuleExports as ModuleExports
import Linter.NoDuplicateTypeclassConstraints as NoDuplicateTypeclassConstraints
import Linter.RecordFormatting as RecordFormatting
import Linter.UnnecessarParenthesis as UnnecessarParenthesis
import Linter.UnnecessaryDo as UnnecessaryDo
import Linter.UseAnonymous as UseAnonymous
import Linter.UsePunning as UsePunning
import Linter.WhereClause as WhereClause
import Test.Common (assertCode, simpleModulePrefix)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

linters :: Spec Unit
linters = describe "Linters" do
  for_
    [ AlignedParenthesis.linter
    , ArrayFormatting.linter
    , IfThenElse.ifThenElseLeftAligned
    , LetBinding.compact
    , NoDuplicateTypeclassConstraints.linter
    , RecordFormatting.linter
    , UnnecessaryDo.linter
    , UnnecessarParenthesis.linter
    , UseAnonymous.forOperations
    , UseAnonymous.forRecordUpdates
    , UseAnonymous.forRecordCreation
    , UsePunning.linter
    , WhereClause.whereLeftAligned
    ]
    $ testLinterWithCode (simpleModulePrefix <> _)

  for_
    [ ModuleExports.exportsRequired
    ]
    $ testLinterWithCode identity
  where
  testLinterWithCode f linter =
    describe (Linter.name linter) do
      let examples = Linter.examples linter
      describe "Examples of Failing Code" do
        for_ examples.bad \code ->
          it code $ assertCode (f code) \m -> (runLintProducer (Linter.defaultLintProducer linter) m) `shouldNotEqual` []
      describe "Examples of Passing Code" do
        for_ examples.good \code ->
          it code $ assertCode (f code) \m -> (runLintProducer (Linter.defaultLintProducer linter) m) `shouldEqual` []

