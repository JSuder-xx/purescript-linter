module CLI.CommandLineOptions
  ( CommandLineOptions
  , RunMode(..)
  , commandLineOptions
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (note)
import Data.Foldable (fold)
import Data.String.NonEmpty as NonEmptyString
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Effect (Effect)
import Options.Applicative (Parser, ParserInfo, ReadM, eitherReader, execParser, flag, flag', fullDesc, header, help, helper, info, long, option, progDesc, short, value, (<**>))

type CommandLineOptions =
  { configFile :: NonEmptyString
  , runMode :: RunMode
  }

data RunMode
  = InitConfig
  | ShowRulesAsAnsi
  | ShowRulesAsMarkdown
  | GenerateRuleJsonSchema
  | LintAllFiles
  | LintSingleFile NonEmptyString

commandLineOptions :: Effect CommandLineOptions
commandLineOptions = execParser commandLineOptionsParserInfo

commandLineOptionsParserInfo :: ParserInfo CommandLineOptions
commandLineOptionsParserInfo = info (commandLineOptionsParser <**> helper)
  ( fullDesc
      <> progDesc "Lint PureScript files"
      <> header "PureScript Linter"
  )

commandLineOptionsParser :: Parser CommandLineOptions
commandLineOptionsParser = ado
  configFile <- option nonEmptyString $ fold
    [ long "config"
    , short 'c'
    , help "Path to the configuration file. This can be used to override the config file. If excluded then the default 'lint.config.json' is used."
    , value $ NonEmptyString "purs-lint.json"
    ]
  runMode <- runModeParser
  in { runMode, configFile }

runModeParser :: Parser RunMode
runModeParser =
  ( LintSingleFile <$>
      ( option nonEmptyString $ fold
          [ long "file"
          , short 'f'
          , help "Specify a single file to lint."
          ]
      )
  )
    <|>
      ( flag' GenerateRuleJsonSchema $ fold
          [ long "rule-json-schema"
          , help "INTERNAL USE: This emits the json schema for the `rules` property."
          ]
      )
    <|>
      ( flag' ShowRulesAsAnsi $ fold
          [ long "show"
          , help "Print all the available rules to the console formatted with ANSI."
          ]
      )
    <|>
      ( flag' ShowRulesAsMarkdown $ fold
          [ long "show-markdown"
          , help "Print all the available rules to the console formatted as Markdown."
          ]
      )
    <|>
      ( flag LintAllFiles InitConfig $ fold
          [ long "init"
          , help "Initialize a configuration file with default values."
          ]
      )

nonEmptyString :: ReadM NonEmptyString
nonEmptyString = eitherReader (NonEmptyString.fromString >>> note "Missing value")
