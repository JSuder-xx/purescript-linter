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
import Rule (LintProducer, LintResult, LintResults, Rule, runLintProducer)
import Rule.AlignedParenthesis as AlignedParenthesis
import Rule.ArrayFormatting as ArrayFormatting
import Rule.IfThenElse as IfThenElse
import Rule.LetBinding as LetBinding
import Rule.ModuleExports as ModuleExports
import Rule.MonoidSimplifications as MonoidSimplifications
import Rule.NoDuplicateTypeclassConstraints as NoDuplicateTypeclassConstraints
import Rule.RecordFormatting as RecordFormatting
import Rule.UnnecessarParenthesis as UnnecessarParenthesis
import Rule.UnnecessaryDo as UnnecessaryDo
import Rule.UseAnonymous as UseAnonymous
import Rule.UsePunning as UsePunning
import Rule.WhereClause as WhereClause

main :: Effect Unit
main = launchAff_ do
  configFile <- filePathToContents "lint.config.json"
  configFile
    # (parseJson >=> AppConfig.decode knownRules)
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
        let fileResults = { filePath, lintResults: lintModule ruleSet.lintProducer $ parseModule content }
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

knownRules :: Array Rule
knownRules =
  [ AlignedParenthesis.rule
  , ArrayFormatting.rule
  , IfThenElse.ifThenElseLeftAligned
  , LetBinding.compact
  , MonoidSimplifications.replaceMaybeMemptyWithFoldMap
  , MonoidSimplifications.useFoldForRepeatedMappends
  , MonoidSimplifications.useGuardOverIfThenElseMEmpty
  , MonoidSimplifications.useGuardOverIfThenMemptyElse
  , ModuleExports.exportsRequired
  , NoDuplicateTypeclassConstraints.rule
  , RecordFormatting.rule
  , UnnecessarParenthesis.rule
  , UnnecessaryDo.rule
  , UseAnonymous.forOperations
  , UseAnonymous.forRecordUpdates
  , UseAnonymous.forRecordCreation
  , UsePunning.rule
  , WhereClause.whereLeftAligned
  ]
