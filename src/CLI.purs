module CLI where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, underline, withGraphics)
import CLI.AppConfig (AppConfig)
import CLI.AppConfig as AppConfig
import CLI.CommandLineOptions (RunMode(..), commandLineOptions)
import CLI.Reporter (Reporter)
import CLI.Reporter.Console as Console
import Data.Argonaut (parseJson, printJsonDecodeError, stringifyWithIndent)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Set as Set
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Data.Traversable (for, for_, intercalate)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import Linter.ModuleRule (Issue, ModuleIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import Linter.ModuleRules (allModuleRules)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, writeFile)
import Node.FS.Sync (exists)
import Node.Glob.Basic (expandGlobsCwd)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad (PositionedError)
import PureScript.CST.Types as CST

cli :: Aff Unit
cli = do
  cliOptions <- liftEffect commandLineOptions
  case cliOptions.runMode of
    InitConfig -> do
      let fileName = NonEmptyString.toString cliOptions.configFile
      fileNameExists <- liftEffect $ exists fileName
      if fileNameExists then liftEffect $ error $ fileName <> " already exists."
      else contentsToFilePath
        { fileName
        , contents: stringifyWithIndent 2 $ AppConfig.encodeDefault allModuleRules
        }
    ShowRules -> do
      for_ allModuleRules \rule -> do
        outputStyled (underline <> foreground BrightWhite) (ModuleRule.name rule)
        outputStyled (foreground White) (String.trim $ ModuleRule.description rule)
        output ""
      where
      outputStyled g = output <<< withGraphics g
      output = liftEffect <<< log

    LintSingleFile fileToLint -> lint cliOptions { singleFile': Just $ NonEmptyString.toString fileToLint }
    LintAllFiles -> lint cliOptions { singleFile': Nothing }
  where
  lint cliOptions files = do
    configFile <- filePathToContents $ NonEmptyString.toString cliOptions.configFile
    configFile
      # (parseJson >=> AppConfig.decode allModuleRules)
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
          let fileResults = { filePath, issues: findIssues ruleSet.moduleIssueIdentifier $ parseModule content }
          liftEffect $ reporter.fileResults fileResults
          pure fileResults
      liftEffect $ reporter.report files
  where
  findIssues :: ModuleIssueIdentifier -> RecoveredParserResult CST.Module -> Array Issue
  findIssues producer = case _ of
    ParseSucceeded module' -> ModuleRule.identifyModuleIssues producer module'
    ParseSucceededWithErrors _ positionedErrors -> positionedErrorToLintResult <$> NonEmptyArray.toArray positionedErrors
    ParseFailed positionedError -> [ positionedErrorToLintResult positionedError ]

  positionedErrorToLintResult :: PositionedError -> Issue
  positionedErrorToLintResult { error, position } = { message: printParseError error, sourceRange: { start: position, end: position } }
