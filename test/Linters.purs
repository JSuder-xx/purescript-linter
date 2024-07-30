module Test.Linter where

import Prelude

import Data.Traversable (for_)
import Linter (runLintProducer)
import Linter.ArrayFormatting as ArrayFormatting
import Linter.CompactLetBinding as CompactLetBinding
import Linter.NoDuplicateTypeclassConstraints as NoDuplicateTypeclassConstraints
import Linter.RecordFormatting as RecordFormatting
import Linter.UnnecessarParenthesis as UnnecessarParenthesis
import Linter.UnnecessaryDo as UnnecessaryDo
import Linter.UseAnonymous as UseAnonymous
import Linter.UsePunning as UsePunning
import Test.Common (assertCode)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

linters :: Spec Unit
linters = describe "Linters" do
  for_
    [ ArrayFormatting.linter
    , CompactLetBinding.linter
    , NoDuplicateTypeclassConstraints.linter
    , RecordFormatting.linter
    , UnnecessaryDo.linter
    , UnnecessarParenthesis.linter
    , UseAnonymous.forOperations
    , UseAnonymous.forRecordUpdates
    , UseAnonymous.forRecordCreation
    , UsePunning.linter
    ]
    \linter -> describe linter.name do
      describe "Examples of Failing Code" do
        for_ linter.examples.bad \code ->
          it code $ assertCode code \m -> (runLintProducer linter.lintProducer m) `shouldNotEqual` []
      describe "Examples of Passing Code" do
        for_ linter.examples.good \code ->
          it code $ assertCode code \m -> (runLintProducer linter.lintProducer m) `shouldEqual` []

