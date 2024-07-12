module Test.Main where

import Prelude

import Data.Array as Array
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Linter (runLintProducer)
import Linter.ArrayFormatting as ArrayFormatting
import Linter.NoDuplicateTypeclassConstraints as NoDuplicateTypeclassConstraints
import Linter.RecordFormatting as RecordFormatting
import Linter.UnnecessarParenthesis as UnnecessarParenthesis
import Linter.UnnecessaryDo as UnnecessaryDo
import Linter.UsePunning as UsePunning
import PureScript.CST (RecoveredParserResult(..), parseModule)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldNotEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

prefix :: String
prefix = "module X where\n\n"

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Linters" do
    for_
      [ ArrayFormatting.linter
      , NoDuplicateTypeclassConstraints.linter
      , RecordFormatting.linter
      , UnnecessaryDo.linter
      , UnnecessarParenthesis.linter
      , UsePunning.linter
      ]
      \linter -> describe linter.name do
        describe "Examples of Failing Code" do
          for_ linter.examples.bad \code ->
            it code do
              case (parseModule $ prefix <> code) of
                ParseSucceeded m -> Array.length (runLintProducer linter.lintProducer m) `shouldNotEqual` 0
                ParseSucceededWithErrors _ _ -> fail "Failed to parse"
                ParseFailed _ -> fail "Failed to parse"
        describe "Examples of Passing Code" do
          for_ linter.examples.good \code ->
            it code do
              case (parseModule $ prefix <> code) of
                ParseSucceeded m -> Array.length (runLintProducer linter.lintProducer m) `shouldEqual` 0
                ParseSucceededWithErrors _ _ -> fail "Failed to parse"
                ParseFailed _ -> fail "Failed to parse"

