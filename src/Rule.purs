module Rule
  ( Examples
  , Rule
  , Rule'
  , LintProducer
  , LintResults
  , LintResult
  , OnKind
  , declarationLintProducer
  , decodeLintProducer
  , defaultLintProducer
  , examples
  , expressionLintProducer
  , mkWithNoConfig
  , moduleLintProducer
  , name
  , runLintProducer
  , typeLintProducer
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson)
import Data.Either (Either)
import PureScript.CST.Fold (OnModule, OnPureScript)
import PureScript.CST.Traversal (foldMapModule)
import PureScript.CST.Types as CST

type LintResult = { message :: String, sourceRange :: CST.SourceRange }
type LintResults = Array LintResult
type OnKind f = f Void -> LintResults

type LintProducer = OnModule LintResults

type Examples =
  { good :: Array String
  , bad :: Array String
  }

newtype Rule = Rule (forall result. (forall config. DecodeJson config => Rule' config -> result) -> result)

type Rule' config =
  { name :: String
  , examples :: Examples
  , defaultConfig :: config
  , lintProducer :: config -> LintProducer
  }

mkRule :: forall config. DecodeJson config => Rule' config -> Rule
mkRule rule = Rule \extract -> extract rule

unRule :: forall result. (forall config. DecodeJson config => Rule' config -> result) -> Rule -> result
unRule f (Rule rule) = rule f

name :: Rule -> String
name = unRule _.name

examples :: Rule -> Examples
examples = unRule _.examples

mkWithNoConfig :: { name :: String, examples :: Examples, lintProducer :: LintProducer } -> Rule
mkWithNoConfig { name: name', examples: examples', lintProducer } =
  mkRule
    { name: name'
    , examples: examples'
    , defaultConfig: unit
    , lintProducer: const lintProducer
    }

decodeLintProducer :: Json -> Rule -> Either JsonDecodeError LintProducer
decodeLintProducer json = unRule \rule -> decodeJson json <#> rule.lintProducer

defaultLintProducer :: Rule -> LintProducer
defaultLintProducer = unRule \rule -> rule.lintProducer rule.defaultConfig

runLintProducer :: LintProducer -> CST.Module Void -> LintResults
runLintProducer { onModule, onPureScript } = onModule <> foldMapModule onPureScript

expressionLintProducer :: OnKind CST.Expr -> LintProducer
expressionLintProducer onExpr = { onModule: mempty, onPureScript: (mempty :: OnPureScript LintResults) { onExpr = onExpr } }

declarationLintProducer :: OnKind CST.Declaration -> LintProducer
declarationLintProducer onDecl = { onModule: mempty, onPureScript: (mempty :: OnPureScript LintResults) { onDecl = onDecl } }

moduleLintProducer :: OnKind CST.Module -> LintProducer
moduleLintProducer onModule = { onModule, onPureScript: (mempty :: OnPureScript LintResults) }

typeLintProducer :: OnKind CST.Type -> LintProducer
typeLintProducer onType = { onModule: mempty, onPureScript: (mempty :: OnPureScript LintResults) { onType = onType } }