module Main where

import Prelude

import AppConfig (AppConfig)
import AppConfig as AppConfig
import CommandLineOptions (RunMode(..), commandLineOptions)
import Data.Argonaut (parseJson, printJsonDecodeError, stringifyWithIndent)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Set as Set
import Data.String.NonEmpty as NonEmptyString
import Data.Traversable (for, for_, intercalate)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (error)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, writeFile)
import Node.FS.Sync (exists)
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
  options <- liftEffect commandLineOptions
  case options.runMode of
    InitConfig -> do
      let fileName = NonEmptyString.toString options.configFile
      fileNameExists <- liftEffect $ exists fileName
      if fileNameExists then liftEffect $ error $ fileName <> " already exists."
      else contentsToFilePath
        { fileName
        , contents: stringifyWithIndent 2 $ AppConfig.encodeDefault knownRules
        }
    LintSingleFile fileToLint -> lint options { singleFile': Just $ NonEmptyString.toString fileToLint }
    LintAllFiles -> lint options { singleFile': Nothing }
  where
  lint options files = do
    configFile <- filePathToContents $ NonEmptyString.toString options.configFile
    configFile
      # (parseJson >=> AppConfig.decode knownRules)
      # either
          (liftEffect <<< error <<< printJsonDecodeError)
          \appConfig -> runLinter files appConfig $ Console.reporter { hideSuccess: appConfig.hideSuccess }

filePathToContents :: String -> Aff String
filePathToContents =
  (liftEffect <<< Buffer.toString UTF8) <=< readFile

contentsToFilePath :: { fileName :: String, contents :: String } -> Aff Unit
contentsToFilePath { fileName, contents } =
  writeFile fileName =<< (liftEffect $ Buffer.fromString contents UTF8)

runLinter :: { singleFile' :: Maybe String } -> AppConfig -> Reporter Effect -> Aff Unit
runLinter { singleFile' } { ruleSets } reporter =
  for_ ruleSets \ruleSet -> do
    filePathSet <- expandGlobsCwd ruleSet.globs
    if filePathSet == mempty && Maybe.isNothing singleFile' then
      liftEffect $ reporter.error $ "No Files found with globs: " <> (intercalate ", " ruleSet.globs)
    else do
      files <- for
        ( Set.toUnfoldable $ singleFile' # Maybe.maybe
            filePathSet
            (Set.intersection filePathSet <<< Set.singleton)
        )
        \filePath -> do
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
