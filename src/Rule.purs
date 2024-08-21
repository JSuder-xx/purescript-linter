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
  , defaultConfigJson
  , defaultLintProducer
  , examples
  , allExpressionsLintProducer
  , mkWithNoConfig
  , moduleLintProducer
  , name
  , runLintProducer
  , typeLintProducer
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

type LintResult = { message :: String, sourceRange :: CST.SourceRange }
type LintResults = Array LintResult
type OnKind f = f Void -> LintResults

type LintProducer = OnModule LintResults

type Examples =
  { good :: Array String
  , bad :: Array String
  }

newtype Rule = Rule (forall result. (forall config. EncodeJson config => DecodeJson config => Rule' config -> result) -> result)

type Rule' config =
  { name :: String
  , examples :: Examples
  , defaultConfig :: config
  , lintProducer :: config -> LintProducer
  }

mkRule :: forall config. EncodeJson config => DecodeJson config => Rule' config -> Rule
mkRule rule = Rule \extract -> extract rule

unRule :: forall result. (forall config. EncodeJson config => DecodeJson config => Rule' config -> result) -> Rule -> result
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

defaultConfigJson :: Rule -> Json
defaultConfigJson = unRule \rule -> encodeJson rule.defaultConfig

defaultLintProducer :: Rule -> LintProducer
defaultLintProducer = unRule \rule -> rule.lintProducer rule.defaultConfig

runLintProducer :: LintProducer -> CST.Module Void -> LintResults
runLintProducer { onModule, onPureScript } = onModule <> foldMapModule onPureScript

-- | Traverses every single expression, traversing into Application to get more expressions.
allExpressionsLintProducer :: OnKind CST.Expr -> LintProducer
allExpressionsLintProducer onExpr = { onModule: mempty, onPureScript: (mempty :: OnPureScript LintResults) { onExpr = recurse } }
  where
  recurse = case _ of
    appExpr@(ExprApp expr nes) -> onExpr appExpr <> onExpr expr <> (NonEmptyArray.toArray nes # Array.mapMaybe Expr.appTerm >>= recurse)
    expr -> onExpr expr

declarationLintProducer :: OnKind CST.Declaration -> LintProducer
declarationLintProducer onDecl = { onModule: mempty, onPureScript: (mempty :: OnPureScript LintResults) { onDecl = onDecl } }

moduleLintProducer :: OnKind CST.Module -> LintProducer
moduleLintProducer onModule = { onModule, onPureScript: (mempty :: OnPureScript LintResults) }

typeLintProducer :: OnKind CST.Type -> LintProducer
typeLintProducer onType = { onModule: mempty, onPureScript: (mempty :: OnPureScript LintResults) { onType = onType } }
