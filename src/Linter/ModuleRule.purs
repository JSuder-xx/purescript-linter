module Linter.ModuleRule
  ( Examples
  , Issue
  , IssueIdentifierIn
  , MkModuleIssueIdentifier
  , ModuleIssueIdentifier
  , ModuleRule
  , ModuleRule'
  , RuleCategory(..)
  , SystemConfig
  , category
  , configJsonSchema
  , declarationIssueIdentifierInModule
  , decodeMkModuleIssueIdentifier
  , defaultConfigJson
  , defaultModuleIssueIdentifier
  , description
  , examples
  , exportedDeclarationIssueIdentifier
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
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Array as Array
import Data.Either (Either)
import Foreign.Object (Object)
import Foreign.Object as Object
import PureScript.CST.Fold (OnModule, OnPureScript)
import PureScript.CST.ModuleHeader as ModuleHeader
import PureScript.CST.Traversal (foldMapModule)
import PureScript.CST.Types (ModuleBody(..))
import PureScript.CST.Types as CST

-- | A Module Rule identifies a specific set of problems / issues with code _inside_ a module.
-- |
-- | NOTE: This is existentially quantified so that each rule can have its own ruleConfiguration which can be decoded from the rule set JSON.
newtype ModuleRule = ModuleRule
  (forall result. (forall ruleConfig. EncodeJson ruleConfig => DecodeJson ruleConfig => ModuleRule' ruleConfig -> result) -> result)

type ModuleRule' ruleConfig =
  { name :: String
  , description :: String
  , category :: RuleCategory
  , examples :: Examples
  , configJsonSchema :: Object Json
  , defaultConfig :: ruleConfig
  , moduleIssueIdentifier :: ruleConfig -> MkModuleIssueIdentifier
  }

data RuleCategory
  = Formatting
  | Style

derive instance Eq RuleCategory
derive instance Ord RuleCategory

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
  , includeModuleHeader :: Boolean
  }

-- | Make a rule that features a custom configuration.
mkModuleRule :: forall ruleConfig. EncodeJson ruleConfig => DecodeJson ruleConfig => ModuleRule' ruleConfig -> ModuleRule
mkModuleRule r = ModuleRule \extract -> extract rule
  where
  rule = r { configJsonSchema = Object.insert "description" (encodeString r.description) r.configJsonSchema }

-- | Make a rule that does not store any custom configuration. The rule is still be given the system-wide configuration.
mkWithNoConfig
  :: { name :: String
     , description :: String
     , category :: RuleCategory
     , examples :: Examples
     , moduleIssueIdentifier :: MkModuleIssueIdentifier
     }
  -> ModuleRule
mkWithNoConfig s =
  mkModuleRule
    { name: s.name
    , examples: s.examples
    , category: s.category
    , description: s.description
    , configJsonSchema: Object.fromHomogeneous
        { type: encodeString "null"
        , description: encodeString s.description
        }
    , defaultConfig: unit
    , moduleIssueIdentifier: const s.moduleIssueIdentifier
    }

unModuleRule :: forall result. (forall ruleConfig. EncodeJson ruleConfig => DecodeJson ruleConfig => ModuleRule' ruleConfig -> result) -> ModuleRule -> result
unModuleRule f (ModuleRule rule) = rule f

name :: ModuleRule -> String
name = unModuleRule _.name

configJsonSchema :: ModuleRule -> Object Json
configJsonSchema = unModuleRule _.configJsonSchema

description :: ModuleRule -> String
description = unModuleRule _.description

category :: ModuleRule -> RuleCategory
category = unModuleRule _.category

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
expressionIssueIdentifier onExpr = { onModule: mempty, onPureScript: (mempty :: OnPureScript (Array Issue)) { onExpr = onExpr } }

declarationIssueIdentifierInModule :: IssueIdentifierIn CST.Declaration -> ModuleIssueIdentifier
declarationIssueIdentifierInModule onDecl = { onModule: mempty, onPureScript: (mempty :: OnPureScript (Array Issue)) { onDecl = onDecl } }

moduleIssueIdentifier :: IssueIdentifierIn CST.Module -> ModuleIssueIdentifier
moduleIssueIdentifier onModule = { onModule, onPureScript: (mempty :: OnPureScript (Array Issue)) }

typeIssueIdentifier :: IssueIdentifierIn CST.Type -> ModuleIssueIdentifier
typeIssueIdentifier onType = { onModule: mempty, onPureScript: (mempty :: OnPureScript (Array Issue)) { onType = onType } }

-- | Identify issues in declarations BUT only if exported from the module.
exportedDeclarationIssueIdentifier :: IssueIdentifierIn CST.Declaration -> ModuleIssueIdentifier
exportedDeclarationIssueIdentifier declarationIdentifier = moduleIssueIdentifier \(CST.Module { body: ModuleBody { decls }, header }) ->
  decls # Array.filter (ModuleHeader.isDeclarationExported header) >>= declarationIdentifier
