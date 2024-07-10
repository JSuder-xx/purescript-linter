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

import PureScript.CST.Traversal (foldMapModule)
import PureScript.CST.Types as CST

type LintResult = { message :: String, sourceRange :: CST.SourceRange }
type LintResults = Array LintResult
type OnKind f = f Void -> LintResults

type LintProducer =
  { onModule :: OnKind CST.Module
  , onDecl :: OnKind CST.Declaration
  , onBinder :: OnKind CST.Binder
  , onExpr :: OnKind CST.Expr
  , onType :: OnKind CST.Type
  }

type Linter =
  { name :: String
  , examples ::
      { good :: Array String
      , bad :: Array String
      }
  , lintProducer :: LintProducer
  }

runLintProducer :: LintProducer -> CST.Module Void -> LintResults
runLintProducer { onModule, onExpr, onType, onBinder, onDecl } = onModule <> foldMapModule { onExpr, onType, onBinder, onDecl }

expressionLintProducer :: OnKind CST.Expr -> LintProducer
expressionLintProducer onExpr = (mempty :: LintProducer) { onExpr = onExpr }

declarationLintProducer :: OnKind CST.Declaration -> LintProducer
declarationLintProducer onDecl = (mempty :: LintProducer) { onDecl = onDecl }