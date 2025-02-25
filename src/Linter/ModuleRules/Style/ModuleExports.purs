module Linter.ModuleRules.Style.ModuleExports (exportsRequired) where

import Prelude

import Data.Maybe (Maybe(..))
import Linter.ModuleRule (RuleCategory(..), moduleIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Types (ModuleHeader(..), Name(..))
import PureScript.CST.Types as CST

exportsRequired :: ModuleRule.ModuleRule
exportsRequired = ModuleRule.mkWithNoConfig
  { name: "ModuleExports.Required"
  , category: Style
  , description:
      "Requiring explicit exports ensures that developers are thinking about encapsulation and avoids missing opportunities to minimize the public surface area."
  , examples:
      { includeModuleHeader: true
      , failingCode:
          [ """
module FlimFlam where

x :: Int
x = 1
          """
          ]
      , passingCode:
          [ """
module FlimFlam (x) where

x :: Int
x = 1
          """
          ]
      }
  , moduleIssueIdentifier: const $ moduleIssueIdentifier $ case _ of
      CST.Module { header: ModuleHeader { name: Name { token: { range: sourceRange } }, exports: Nothing } } ->
        [ { message: "Module should have explicit exports.", sourceRange } ]
      _ -> []
  }
