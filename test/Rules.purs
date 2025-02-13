module Test.Rules where

import Prelude

import Data.Traversable (for_)
import Linter.ModuleRule (identifyModuleIssues)
import Linter.ModuleRule as ModuleRule
import Linter.ModuleRules as ModuleRules
import Test.Common (assertCode, simpleModulePrefix)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

main :: Spec Unit
main = describe "Linters" do
  for_ ModuleRules.allModuleRules
    \rule ->
      describe (ModuleRule.name rule) do
        let
          examples = ModuleRule.examples rule
          transformExample = if examples.includeModuleHeader then identity else (simpleModulePrefix <> _)
          config = { indentSpaces: 2 }
        describe "Examples of Failing Code" do
          for_ examples.failingCode \code ->
            it code $ assertCode (transformExample code) \m -> (identifyModuleIssues (ModuleRule.defaultModuleIssueIdentifier rule config) m) `shouldNotEqual` []
        describe "Examples of Passing Code" do
          for_ examples.passingCode \code ->
            it code $ assertCode (transformExample code) \m -> (identifyModuleIssues (ModuleRule.defaultModuleIssueIdentifier rule config) m) `shouldEqual` []
