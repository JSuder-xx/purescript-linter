module Linter
  ( Linter
  , LintProducer
  , LintResults
  , LintResult
  , OnKind
  , declarationLintProducer
  , expressionLintProducer
  , runLintProducer
  ) where

import Prelude
import PureScript.CST.Fold (OnModule, OnPureScript)
import PureScript.CST.Traversal (foldMapModule)
import PureScript.CST.Types as CST

type LintResult = { message :: String, sourceRange :: CST.SourceRange }
type LintResults = Array LintResult
type OnKind f = f Void -> LintResults

type LintProducer = OnModule LintResults

type Linter =
  { name :: String
  , examples ::
      { good :: Array String
      , bad :: Array String
      }
  , lintProducer :: LintProducer
  }

runLintProducer :: LintProducer -> CST.Module Void -> LintResults
runLintProducer { onModule, onPureScript } = onModule <> foldMapModule onPureScript

expressionLintProducer :: OnKind CST.Expr -> LintProducer
expressionLintProducer onExpr = { onModule: mempty, onPureScript: (mempty :: OnPureScript LintResults) { onExpr = onExpr } }

declarationLintProducer :: OnKind CST.Declaration -> LintProducer
declarationLintProducer onDecl = { onModule: mempty, onPureScript: (mempty :: OnPureScript LintResults) { onDecl = onDecl } }
