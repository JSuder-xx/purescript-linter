module Main where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Linter (LintResult, LintResults, LintProducer, runLintProducer)
import Linter.NoDuplicateTypeclassConstraints as NoDuplicateTypeclassConstraints
import Linter.UnnecessarParenthesis as UnnecessarParenthesis
import Linter.UnnecessaryDo as UnnecessaryDo
import Linter.UsePunning as UsePunning
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile)
import Node.Glob.Basic (expandGlobsCwd)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad (PositionedError)
import PureScript.CST.Types as CST

-- Eventually which linters to include will be configured via JSON 
combined :: LintProducer
combined =
  NoDuplicateTypeclassConstraints.linter.lintProducer
    <> UnnecessarParenthesis.linter.lintProducer
    <> UnnecessaryDo.linter.lintProducer
    <> UsePunning.linter.lintProducer

runLinter :: String -> (Either String LintResults -> Effect Unit) -> Effect Unit
runLinter src fn = launchAff_ do
  files <- Set.toUnfoldable <$> expandGlobsCwd [ src ]
  if files == [] then liftEffect $ fn $ Left $ "No Files found with src : " <> src
  else for_ files \filePath -> do
    content <- (liftEffect <<< Buffer.toString UTF8) =<< readFile filePath
    liftEffect $ fn $ Right $ lintModule $ parseModule content
  where
  lintModule :: RecoveredParserResult CST.Module -> LintResults
  lintModule = case _ of
    ParseSucceeded m -> runLintProducer combined m
    ParseSucceededWithErrors _ positionedErrors -> positionedErrorToLintResult <$> NonEmptyArray.toArray positionedErrors
    ParseFailed positionedError -> [ positionedErrorToLintResult positionedError ]

  positionedErrorToLintResult :: PositionedError -> LintResult
  positionedErrorToLintResult { error, position } = { message: printParseError error, sourceRange: { start: position, end: position } }