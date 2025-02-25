module Linter.ModuleRules.Style.UseForallSymbol (rule) where

import Prelude

import Linter.ModuleRule (RuleCategory(..), typeIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Types (SourceStyle(..), Token(..))
import PureScript.CST.Types as CST

rule :: ModuleRule.ModuleRule
rule = ModuleRule.mkWithNoConfig
  { name: "UseForallSymbol"
  , category: Style
  , description: "Some projects prefer using the ∀ symbol rather than 'forall' to improve type signature readability and require less typing."
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ "f :: forall a. a -> a"
          , "f :: forall a b c. a -> b -> c -> a"
          , "f :: forall a b. a -> b -> (forall c. c -> String) -> String"
          , """
f :: forall a b
  .  a
  -> b
  -> (forall c. c -> String)
  -> String
          """
          ]
      , passingCode:
          [ "f :: ∀ a. a -> a"
          , "f :: ∀ a b c. a -> b -> c -> a"
          , "f :: ∀ a b. a -> b -> (∀ c. c -> String) -> String"
          , """
f :: ∀ a b
  .  a
  -> b
  -> (∀ c. c -> String)
  -> String
          """
          ]
      }
  , moduleIssueIdentifier: const $ typeIssueIdentifier $ case _ of
      CST.TypeForall { value: TokForall forAllSourceStyle, range: forAllRange } _variableNames _token _dataType ->
        case forAllSourceStyle of
          ASCII -> [ { message: "Please use ∀ rather than 'forall'.", sourceRange: forAllRange } ]
          Unicode -> []
      _ -> []
  }
