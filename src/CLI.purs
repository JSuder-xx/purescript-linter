module CLI where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, underline, withGraphics)
import CLI.AppConfig (AppConfigProcessed, ProjectRoot, Verbosity(..))
import CLI.AppConfig as AppConfig
import CLI.CommandLineOptions (RunMode(..), commandLineOptions)
import CLI.Reporter (Reporter)
import CLI.Reporter.Console as Reporter.Console
import Data.Argonaut (encodeJson, parseJson, printJsonDecodeError, stringify, stringifyWithIndent)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.DateTime.Instant as Instant
import Data.Either (either)
import Data.Foldable (foldM, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Extra as Map.Extra
import Data.Maybe (Maybe(..), isNothing, maybe)
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
import Effect.Console as Console
import Effect.Now (now)
import Linter.ModuleRule (Issue, ModuleIssueIdentifier, RuleCategory(..))
import Linter.ModuleRule as ModuleRule
import Linter.ModuleRules (allModuleRules, recommendedRules)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, writeFile)
import Node.FS.Sync (exists)
import Node.Glob.Basic (expandGlobsCwd)
import Node.Path (FilePath, sep)
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
      if fileNameExists then liftEffect $ Console.error $ fileName <> " already exists."
      else contentsToFilePath
        { fileName
        , contents: stringifyWithIndent 2 $ encodeJson $ AppConfig.defaultAppConfg recommendedRules
        }
    GenerateRuleJsonSchema -> log $ stringify $ AppConfig.rulesSchema allModuleRules
    ShowRulesAsMarkdown -> do
      log "## Rules"
      showRules
        { categoryHeader: log <<< ("### " <> _)
        , categoryDescription: log
        , ruleHeader: \s -> do
            log $ "#### " <> s
            log ""
        , ruleDescription: \s -> do
            log s
            log ""
        }
    ShowRulesAsAnsi ->
      showRules
        { categoryHeader: outputStyled "" (underline <> foreground BrightWhite)
        , categoryDescription: outputStyled "" (foreground White)
        , ruleHeader: outputStyled "  " (foreground BrightYellow)
        , ruleDescription: \s -> do
            let content = outputStyled "  " (foreground White)
            content s
            content ""
            content ""
        }
      where
      outputStyled indent g s = for_ (String.split (String.Pattern "\n") s) $ \line ->
        log $ withGraphics g $ indent <> line

    LintSingleFile fileToLint -> lint cliOptions { singleFile': Just $ Path.normalize $ NonEmptyString.toString fileToLint }
    LintAllFiles -> lint cliOptions { singleFile': Nothing }
  where
  log = liftEffect <<< Console.log

  showRules { categoryHeader, categoryDescription, ruleHeader, ruleDescription } =
    for_ ((Map.toUnfoldable $ Map.Extra.indexedBy ModuleRule.category allModuleRules) :: Array _) \(Tuple category rules) -> do
      let
        { header, description } = case category of
          Style ->
            { header: "Style"
            , description:
                """Style rules cover code "correctness" or stylistic choices. For example, there is a rule to prefer the use of punning when available.
            """
            }
          Formatting ->
            { header: "Formatting"
            , description:
                """Formatting rules cover lexical formatting concerns that might overwise be handled by a formatter/pretty printer.
Use these rules when unable to use a formatter in your codebase. For example, perhaps a formatter does not conform to prescribed rules."""
            }
      categoryHeader header
      categoryDescription description
      log ""

      for_ (Array.sortWith ModuleRule.name $ NonEmpty.fromNonEmpty Array.cons rules) \rule -> do
        ruleHeader $ ModuleRule.name rule
        ruleDescription $ String.trim $ ModuleRule.description rule
      log ""

  lint cliOptions files = do
    cwd <- liftEffect cwd
    let configFilename = NonEmptyString.toString cliOptions.configFile
    configFile <- filePathToContents configFilename
    configFile
      # (parseJson >>> lmap printJsonDecodeError >=> (AppConfig.decode >>> lmap printJsonDecodeError) >=> AppConfig.rawToProcessed allModuleRules)
      # either
          (liftEffect <<< Console.error <<< \decodeError -> "Error decoding '" <> configFilename <> "'\n" <> decodeError)
          \appConfig -> runLinter { cwd } files (AppConfig.withCwd (Path.normalize cwd) appConfig) $ Reporter.Console.reporter { verbosity: if isNothing files.singleFile' then appConfig.verbosity else Quiet }

simplifyPath :: String -> String -> String
simplifyPath cwd filePath = filePath # String.stripPrefix (Pattern cwd) # maybe filePath ("." <> _)

filePathToContents :: String -> Aff String
filePathToContents =
  (liftEffect <<< Buffer.toString UTF8) <=< readFile

contentsToFilePath :: { fileName :: String, contents :: String } -> Aff Unit
contentsToFilePath { fileName, contents } =
  writeFile fileName =<< (liftEffect $ Buffer.fromString contents UTF8)

runLinter :: { cwd :: String } -> { singleFile' :: Maybe String } -> AppConfigProcessed -> Reporter Effect -> Aff Unit
runLinter { cwd } { singleFile' } { ruleSets, projectRoots } reporter = do
  startNow <- liftEffect now
  liftEffect reporter.indicateStarted
  { warnings, filesMap } <- findAllFiles
  for_ warnings $ liftEffect <<< reporter.error
  fileResults <- for (Map.toUnfoldable filesMap) \(Tuple filePath rules) -> do
    results <- filePathToContents filePath <#> \content ->
      { filePath, issues: findIssues filePath rules $ parseModule content }
    liftEffect $ reporter.indicateFileProcessed results
    pure results
  endNow <- liftEffect now
  liftEffect $ reporter.report (Instant.diff endNow startNow) $ fileResults <#> \r -> r { filePath = simplifyPath cwd r.filePath }
  where
  findAllFiles :: Aff { warnings :: Array String, filesMap :: Map FilePath ModuleIssueIdentifier }
  findAllFiles = foldM processRuleSet { warnings: [], filesMap: Map.empty } ruleSets
    where
    processRuleSet { warnings, filesMap } ruleSet = expandGlobsCwd ruleSet.globs <#> \filePathSet ->
      if filePathSet == mempty && Maybe.isNothing singleFile' then
        { filesMap
        , warnings: Array.snoc warnings $ "No Files found with globs: " <> (intercalate ", " ruleSet.globs)
        }
      else
        { warnings
        , filesMap: foldl (processFile ruleSet.rules) filesMap $ singleFile' # Maybe.maybe filePathSet (Set.intersection filePathSet <<< Set.singleton)
        }

    processFile :: ModuleIssueIdentifier -> Map String ModuleIssueIdentifier -> String -> Map String ModuleIssueIdentifier
    processFile rules map file = Map.insertWith append file rules map

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
