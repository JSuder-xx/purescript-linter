module CLI where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (background, foreground, underline, withGraphics)
import CLI.AppConfig (AppConfig, ProjectRoot)
import CLI.AppConfig as AppConfig
import CLI.CommandLineOptions (RunMode(..), commandLineOptions)
import CLI.Reporter (Reporter)
import CLI.Reporter.Console as Console
import Data.Argonaut (parseJson, printJsonDecodeError, stringifyWithIndent)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime.Instant as Instant
import Data.Either (either)
import Data.Map as Map
import Data.Map.Extra as Map.Extra
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Maybe.Extra as Maybe.Extra
import Data.Monoid (guard)
import Data.NonEmpty as NonEmpty
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Data.Traversable (foldMap, for, for_, intercalate)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import Effect.Now (now)
import Linter.ModuleRule (Issue, ModuleIssueIdentifier, RuleCategory(..))
import Linter.ModuleRule as ModuleRule
import Linter.ModuleRules (allModuleRules)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, writeFile)
import Node.FS.Sync (exists)
import Node.Glob.Basic (expandGlobsCwd)
import Node.Path (sep)
import Node.Path as Path
import Node.Process (cwd)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad (PositionedError)
import PureScript.CST.Types (Module(..), ModuleHeader(..), ModuleName(..), Name(..))
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
    ShowRules ->
      for_ ((Map.toUnfoldable $ Map.Extra.indexedBy ModuleRule.category allModuleRules) :: Array _) \(Tuple category rules) -> do
        outputStyled "" (underline <> foreground BrightWhite <> background Black) case category of
          Style -> "Style"
          Formatting -> "Formatting"
        for_ (Array.sortWith ModuleRule.name $ NonEmpty.fromNonEmpty Array.cons rules) \rule -> do
          outputStyled "  " (foreground BrightBlue <> background Black) (ModuleRule.name rule)
          outputStyled "  " (foreground White <> background Black) (String.trim $ ModuleRule.description rule)
          output "" ""
          output "" ""
        output "" ""
      where
      outputStyled indent g = output indent <<< withGraphics g
      output indent s = for_ (String.split (String.Pattern "\n") s) $ \line -> liftEffect $ log $ indent <> line

    LintSingleFile fileToLint -> lint cliOptions { singleFile': Just $ NonEmptyString.toString fileToLint }
    LintAllFiles -> lint cliOptions { singleFile': Nothing }
  where
  lint cliOptions files = do
    cwd <- liftEffect cwd
    configFile <- filePathToContents $ NonEmptyString.toString cliOptions.configFile
    configFile
      # (parseJson >=> AppConfig.decode allModuleRules)
      # either
          (liftEffect <<< error <<< printJsonDecodeError)
          \appConfig -> runLinter files (AppConfig.withCwd (Path.normalize cwd) appConfig) $ Console.reporter { hideSuccess: appConfig.hideSuccess }

filePathToContents :: String -> Aff String
filePathToContents =
  (liftEffect <<< Buffer.toString UTF8) <=< readFile

contentsToFilePath :: { fileName :: String, contents :: String } -> Aff Unit
contentsToFilePath { fileName, contents } =
  writeFile fileName =<< (liftEffect $ Buffer.fromString contents UTF8)

runLinter :: { singleFile' :: Maybe String } -> AppConfig -> Reporter Effect -> Aff Unit
runLinter { singleFile' } { ruleSets, projectRoots } reporter =
  for_ ruleSets \ruleSet -> do
    startNow <- liftEffect now
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
          let fileResults = { filePath, issues: findIssues filePath ruleSet.moduleIssueIdentifier $ parseModule content }
          liftEffect $ reporter.fileResults fileResults
          pure fileResults
      endNow <- liftEffect now
      liftEffect $ reporter.report (Instant.diff endNow startNow) files
  where
  findIssues :: String -> ModuleIssueIdentifier -> RecoveredParserResult CST.Module -> Array Issue
  findIssues filePath producer = case _ of
    ParseSucceeded moduleCst@(Module { header: ModuleHeader { name: moduleName } }) ->
      issuesWithModuleName filePath moduleName projectRoots <> ModuleRule.identifyModuleIssues producer moduleCst
    ParseSucceededWithErrors _ positionedErrors -> positionedErrorToLintResult <$> NonEmptyArray.toArray positionedErrors
    ParseFailed positionedError -> [ positionedErrorToLintResult positionedError ]

  issuesWithModuleName :: String -> Name ModuleName -> Array ProjectRoot -> Array Issue
  issuesWithModuleName filePath (Name { name: ModuleName moduleName, token: { range } }) =
    Array.findMap (\{ pathPrefix, modulePrefix } -> String.stripPrefix (Pattern pathPrefix) filePath <#> { remainingPath: _, modulePrefix })
      >>> foldMap \{ modulePrefix, remainingPath } ->
        let
          expected = modulePrefix <> pathToModule remainingPath
        in
          guard (expected /= moduleName) [ { message: "Expecting module named '" <> moduleName <> "' to be named '" <> expected <> "' based on its path.", sourceRange: range } ]

  pathToModule :: String -> String
  pathToModule = (Maybe.Extra.recover $ String.stripPrefix (Pattern ".")) <<< (Maybe.Extra.recover $ String.stripSuffix (Pattern ".purs")) <<< String.replaceAll (Pattern sep) (Replacement ".")

  positionedErrorToLintResult :: PositionedError -> Issue
  positionedErrorToLintResult { error, position } = { message: printParseError error, sourceRange: { start: position, end: position } }
