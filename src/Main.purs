module Main where

import Prelude

import Data.Array (foldMap)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Set as Set
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Linter (LintResult, LintResults, LintProducer, runLintProducer)
import Linter.ArrayFormatting as ArrayFormatting
import Linter.LetBinding as LetBinding
import Linter.NoDuplicateTypeclassConstraints as NoDuplicateTypeclassConstraints
import Linter.RecordFormatting as RecordFormatting
import Linter.UnnecessarParenthesis as UnnecessarParenthesis
import Linter.UnnecessaryDo as UnnecessaryDo
import Linter.UseAnonymous as UseAnonymous
import Linter.UsePunning as UsePunning
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile)
import Node.Glob.Basic (expandGlobsCwd)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad (PositionedError)
import PureScript.CST.Types as CST
import Reporter (Reporter)
import Reporter.Console as Console

main :: Effect Unit
main = runLinter "**/*.purs" $ Console.reporter { hideSuccess: true }

-- Eventually which linters to include will be configured via JSON 
combined :: LintProducer
combined = foldMap _.lintProducer
  [ ArrayFormatting.linter
  , LetBinding.compact
  , NoDuplicateTypeclassConstraints.linter
  , RecordFormatting.linter
  , UnnecessarParenthesis.linter
  , UnnecessaryDo.linter
  , UseAnonymous.forOperations
  , UseAnonymous.forRecordUpdates
  , UseAnonymous.forRecordCreation
  , UsePunning.linter
  ]

runLinter :: String -> Reporter Effect -> Effect Unit
runLinter src reporter = launchAff_ do
  filePaths <- Set.toUnfoldable <$> expandGlobsCwd [ src ]
  if filePaths == [] then
    liftEffect $ reporter.error $ "No Files found with src : " <> src
  else do
    files <- for filePaths \filePath -> do
      content <- (liftEffect <<< Buffer.toString UTF8) =<< readFile filePath
      let fileResults = { filePath, lintResults: lintModule $ parseModule content }
      liftEffect $ reporter.fileResults fileResults
      pure fileResults
    liftEffect $ reporter.report files

  where
  lintModule :: RecoveredParserResult CST.Module -> LintResults
  lintModule = case _ of
    ParseSucceeded m -> runLintProducer combined m
    ParseSucceededWithErrors _ positionedErrors -> positionedErrorToLintResult <$> NonEmptyArray.toArray positionedErrors
    ParseFailed positionedError -> [ positionedErrorToLintResult positionedError ]

  positionedErrorToLintResult :: PositionedError -> LintResult
  positionedErrorToLintResult { error, position } = { message: printParseError error, sourceRange: { start: position, end: position } }
