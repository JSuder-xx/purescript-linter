module Linter.ModuleRule
  ( Examples
  , IssueIdentifierIn
  , MkModuleIssueIdentifier
  , ModuleIssueIdentifier
  , Issue
  , ModuleRule
  , ModuleRule'
  , SystemConfig
  , declarationIssueIdentifierInModule
  , decodeMkModuleIssueIdentifier
  , defaultConfigJson
  , defaultModuleIssueIdentifier
  , description
  , examples
  , expressionIssueIdentifier
  , mkModuleRule
  , mkWithNoConfig
  , moduleIssueIdentifier
  , name
  , identifyModuleIssues
  , typeIssueIdentifier
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either)
import PureScript.CST.Expr as Expr
import PureScript.CST.Fold (OnModule, OnPureScript)
import PureScript.CST.Traversal (foldMapModule)
import PureScript.CST.Types (Expr(..))
import PureScript.CST.Types as CST

-- | A Module Rule identifies a specific set of problems / issues with code _inside_ a module.
-- |
-- | NOTE: This is existentially quantified so that each rule can have its own ruleConfiguration which can be decoded from the rule set JSON.
newtype ModuleRule = ModuleRule
  (forall result. (forall ruleConfig. EncodeJson ruleConfig => DecodeJson ruleConfig => ModuleRule' ruleConfig -> result) -> result)

type ModuleRule' ruleConfig =
  { name :: String
  , description :: String
  , examples :: Examples
  , defaultConfig :: ruleConfig
  , moduleIssueIdentifier :: ruleConfig -> MkModuleIssueIdentifier
  }

type Issue = { message :: String, sourceRange :: CST.SourceRange }

-- | Function that return issues for a given CST node.
type IssueIdentifierIn cstType = cstType Void -> Array Issue

-- | A record of functions that identify issues in different parts of a Module ex. types, declarations, expressions, imports/exports, etc..
type ModuleIssueIdentifier = OnModule (Array Issue)

-- | System level configuration provided to all Rules.
type SystemConfig = { indentSpaces :: Int }

type MkModuleIssueIdentifier = SystemConfig -> ModuleIssueIdentifier

-- | Each rule should provide examples of code that would fail the rule and code that would pass.
type Examples =
  { passingCode :: Array String
  , failingCode :: Array String
  }

-- | Make a rule that features a custom configuration.
mkModuleRule :: forall ruleConfig. EncodeJson ruleConfig => DecodeJson ruleConfig => ModuleRule' ruleConfig -> ModuleRule
mkModuleRule rule = ModuleRule \extract -> extract rule

-- | Make a rule that does not store any custom configuration. The rule is still be given the system-wide configuration.
mkWithNoConfig :: { name :: String, description :: String, examples :: Examples, moduleIssueIdentifier :: MkModuleIssueIdentifier } -> ModuleRule
mkWithNoConfig s =
  mkModuleRule
    { name: s.name
    , examples: s.examples
    , description: s.description
    , defaultConfig: unit
    , moduleIssueIdentifier: const s.moduleIssueIdentifier
    }

unModuleRule :: forall result. (forall ruleConfig. EncodeJson ruleConfig => DecodeJson ruleConfig => ModuleRule' ruleConfig -> result) -> ModuleRule -> result
unModuleRule f (ModuleRule rule) = rule f

name :: ModuleRule -> String
name = unModuleRule _.name

description :: ModuleRule -> String
description = unModuleRule _.description

examples :: ModuleRule -> Examples
examples = unModuleRule _.examples

decodeMkModuleIssueIdentifier :: Json -> ModuleRule -> Either JsonDecodeError MkModuleIssueIdentifier
decodeMkModuleIssueIdentifier json = unModuleRule \rule -> decodeJson json <#> rule.moduleIssueIdentifier

defaultConfigJson :: ModuleRule -> Json
defaultConfigJson = unModuleRule \rule -> encodeJson rule.defaultConfig

defaultModuleIssueIdentifier :: ModuleRule -> MkModuleIssueIdentifier
defaultModuleIssueIdentifier = unModuleRule \rule -> rule.moduleIssueIdentifier rule.defaultConfig

identifyModuleIssues :: ModuleIssueIdentifier -> CST.Module Void -> Array Issue
identifyModuleIssues { onModule, onPureScript } = onModule <> foldMapModule onPureScript

-- | Traverses every single expression, traversing into Application to get more expressions.
expressionIssueIdentifier :: IssueIdentifierIn CST.Expr -> ModuleIssueIdentifier
expressionIssueIdentifier onExpr = { onModule: mempty, onPureScript: (mempty :: OnPureScript (Array Issue)) { onExpr = recurse } }
  where
  recurse = case _ of
    appExpr@(ExprApp expr nes) -> onExpr appExpr <> onExpr expr <> (NonEmptyArray.toArray nes # Array.mapMaybe Expr.appTerm >>= recurse)
    expr -> onExpr expr

declarationIssueIdentifierInModule :: IssueIdentifierIn CST.Declaration -> ModuleIssueIdentifier
declarationIssueIdentifierInModule onDecl = { onModule: mempty, onPureScript: (mempty :: OnPureScript (Array Issue)) { onDecl = onDecl } }

moduleIssueIdentifier :: IssueIdentifierIn CST.Module -> ModuleIssueIdentifier
moduleIssueIdentifier onModule = { onModule, onPureScript: (mempty :: OnPureScript (Array Issue)) }

typeIssueIdentifier :: IssueIdentifierIn CST.Type -> ModuleIssueIdentifier
typeIssueIdentifier onType = { onModule: mempty, onPureScript: (mempty :: OnPureScript (Array Issue)) { onType = onType } }
