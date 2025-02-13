module Linter.ModuleRules.Style.ModuleImports where

import Prelude

import Data.Foldable (foldMap)
import Data.Matcher (Matcher(..))
import Data.Matcher as Matcher
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Linter.ModuleRule (ModuleRule, RuleCategory(..), mkModuleRule, moduleIssueIdentifier)
import PureScript.CST.Import as Import
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Separated as Separated
import PureScript.CST.Types (ImportDecl(..), ModuleName(..), Name(..), Wrapped(..))
import PureScript.CST.Types as CST

qualification :: ModuleRule
qualification = mkModuleRule
  { name: "ModuleImportQualification"
  , description: ""
  , category: Style
  , examples:
      { includeModuleHeader: true
      , passingCode:
          includeModuleName <$>
            [ "import Data.Array (blurble)" -- single import not on the list
            , "import Data.Array (foo, bar, baz)" -- multiple imports not on the list
            , "import Data.Array (fromFoldable) as Array" -- function on the list but module qualified
            , "import Data.Whatever (fromFoldable)" -- import on the list but module not a match
            , "import Data.Whatever" -- module name not on list so open ended is allowed
            ]
      , failingCode:
          includeModuleName <$>
            [ "import Data.Array (fromFoldable)" -- single module with a single failing import
            , "import Data.Array (foo, bar, fromFoldable)" -- single module with two passing imports
            , "import Data.Array (foo, fromFoldable, bar, toUnfoldable)" -- single module with two passing and two failing imports
            , "import Data.Array" -- failed because open ended module and on the list of modules to match
            ]
      }
  , defaultConfig:
      [ { module: MatchExact
            [ "Data.Array"
            , "Data.Array.NonEmpty"
            , "Data.Either"
            , "Data.Lens"
            , "Data.List"
            , "Data.List.Lazy"
            , "Data.Map"
            , "Data.Maybe"
            , "Data.Set"
            , "Foreign.Object"
            , "JSON.Object"
            ]
        , import: MatchExact
            [ "fromFoldable"
            , "toUnfoldable"
            , "fromString"
            , "singleton"
            , "member"
            , "over"
            , "set"
            , "get"
            ]
        }
      ] :: Array ImportMatch
  , moduleIssueIdentifier: \importMatches _systemConfig -> moduleIssueIdentifier $ \(CST.Module { header: CST.ModuleHeader { imports } }) -> do
      importMatch <- importMatches
      verifyImportDeclaration importMatch =<< imports
  }
  where
  includeModuleName s = "module Test where\n" <> s

  verifyImportDeclaration _ (ImportDecl { qualified: Just _ }) = []
  verifyImportDeclaration importMatch (importDecl@(ImportDecl { module: Name { name: ModuleName moduleName }, names: Nothing })) =
    guard (Matcher.matches moduleName importMatch.module)
      [ { message: "Cannot import open ended when requiring qualification and the module name matches.", sourceRange: rangeOf importDecl } ]
  verifyImportDeclaration importMatch (ImportDecl { module: Name { name: ModuleName moduleName }, names: Just (Tuple _sourceToken (Wrapped { value: separatedImports })) }) =
    guard (Matcher.matches moduleName importMatch.module) (separatedImports # Separated.values >>= verifyImport importMatch.import)

  verifyImport moduleMatch import' = import' # Import.name' # foldMap \importName ->
    guard (Matcher.matches importName moduleMatch) [ { message: "Do not", sourceRange: rangeOf import' } ]

type ImportMatch =
  { module :: Matcher
  , import :: Matcher
  }
