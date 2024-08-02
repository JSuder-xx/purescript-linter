module Linter
  ( Examples
  , Linter
  , Linter'
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
  , name
  , runLintProducer
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

newtype Linter = Linter (forall result. (forall config. DecodeJson config => Linter' config -> result) -> result)

type Linter' config =
  { name :: String
  , examples :: Examples
  , defaultConfig :: config
  , lintProducer :: config -> LintProducer
  }

mkLinter :: forall config. DecodeJson config => Linter' config -> Linter
mkLinter linter = Linter \extract -> extract linter

unLinter :: forall result. (forall config. DecodeJson config => Linter' config -> result) -> Linter -> result
unLinter f (Linter linter) = linter f

name :: Linter -> String
name = unLinter _.name

examples :: Linter -> Examples
examples = unLinter _.examples

mkWithNoConfig :: { name :: String, examples :: Examples, lintProducer :: LintProducer } -> Linter
mkWithNoConfig { name: name', examples: examples', lintProducer } =
  mkLinter
    { name: name'
    , examples: examples'
    , defaultConfig: unit
    , lintProducer: const lintProducer
    }

decodeLintProducer :: Json -> Linter -> Either JsonDecodeError LintProducer
decodeLintProducer json = unLinter \linter -> decodeJson json <#> linter.lintProducer

defaultLintProducer :: Linter -> LintProducer
defaultLintProducer = unLinter \linter -> linter.lintProducer linter.defaultConfig

runLintProducer :: LintProducer -> CST.Module Void -> LintResults
runLintProducer { onModule, onPureScript } = onModule <> foldMapModule onPureScript

expressionLintProducer :: OnKind CST.Expr -> LintProducer
expressionLintProducer onExpr = { onModule: mempty, onPureScript: (mempty :: OnPureScript LintResults) { onExpr = onExpr } }

declarationLintProducer :: OnKind CST.Declaration -> LintProducer
declarationLintProducer onDecl = { onModule: mempty, onPureScript: (mempty :: OnPureScript LintResults) { onDecl = onDecl } }
