module Main where

import Prelude

import AppConfig (AppConfig)
import AppConfig as AppConfig
import Data.Argonaut (parseJson, printJsonDecodeError)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (either)
import Data.Set as Set
import Data.Traversable (for, for_, intercalate)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (error)
import Linter (LintProducer, LintResult, LintResults, Linter, runLintProducer)
import Linter.AlignedParenthesis as AlignedParenthesis
import Linter.ArrayFormatting as ArrayFormatting
import Linter.IfThenElse as IfThenElse
import Linter.LetBinding as LetBinding
import Linter.ModuleExports as ModuleExports
import Linter.NoDuplicateTypeclassConstraints as NoDuplicateTypeclassConstraints
import Linter.RecordFormatting as RecordFormatting
import Linter.UnnecessarParenthesis as UnnecessarParenthesis
import Linter.UnnecessaryDo as UnnecessaryDo
import Linter.UseAnonymous as UseAnonymous
import Linter.UsePunning as UsePunning
import Linter.WhereClause as WhereClause
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
main = launchAff_ do
  configFile <- filePathToContents "lint.config.json"
  configFile
    # (parseJson >=> AppConfig.decode knownLinters)
    # either
        (liftEffect <<< error <<< printJsonDecodeError)
        \appConfig -> runLinter appConfig $ Console.reporter { hideSuccess: appConfig.hideSuccess }

filePathToContents :: String -> Aff String
filePathToContents = (liftEffect <<< Buffer.toString UTF8) <=< readFile

runLinter :: AppConfig -> Reporter Effect -> Aff Unit
runLinter { ruleSets } reporter = do
  for_ ruleSets \ruleSet -> do
    filePaths <- Set.toUnfoldable <$> expandGlobsCwd ruleSet.globs
    if filePaths == [] then
      liftEffect $ reporter.error $ "No Files found with globs: " <> (intercalate ", " ruleSet.globs)
    else do
      files <- for filePaths \filePath -> do
        content <- filePathToContents filePath
        let fileResults = { filePath, lintResults: lintModule ruleSet.linter $ parseModule content }
        liftEffect $ reporter.fileResults fileResults
        pure fileResults
      liftEffect $ reporter.report files

  where
  lintModule :: LintProducer -> RecoveredParserResult CST.Module -> LintResults
  lintModule producer = case _ of
    ParseSucceeded m -> runLintProducer producer m
    ParseSucceededWithErrors _ positionedErrors -> positionedErrorToLintResult <$> NonEmptyArray.toArray positionedErrors
    ParseFailed positionedError -> [ positionedErrorToLintResult positionedError ]

  positionedErrorToLintResult :: PositionedError -> LintResult
  positionedErrorToLintResult { error, position } = { message: printParseError error, sourceRange: { start: position, end: position } }

knownLinters :: Array Linter
knownLinters =
  [ AlignedParenthesis.linter
  , ArrayFormatting.linter
  , IfThenElse.ifThenElseLeftAligned
  , LetBinding.compact
  , ModuleExports.exportsRequired
  , NoDuplicateTypeclassConstraints.linter
  , RecordFormatting.linter
  , UnnecessarParenthesis.linter
  , UnnecessaryDo.linter
  , UseAnonymous.forOperations
  , UseAnonymous.forRecordUpdates
  , UseAnonymous.forRecordCreation
  , UsePunning.linter
  , WhereClause.whereLeftAligned
  ]
