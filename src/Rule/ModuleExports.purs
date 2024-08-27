module Rule.ModuleExports (exportsRequired) where

import Prelude

import Data.Maybe (Maybe(..))
import Rule (moduleLintProducer)
import Rule as Rule
import PureScript.CST.Types (ModuleHeader(..), Name(..))
import PureScript.CST.Types as CST

exportsRequired :: Rule.Rule
exportsRequired = Rule.mkWithNoConfig
  { name: "ModuleExportsRequired"
  , description:
      "Requiring explicit exports ensures that developers are thinking about encapsulation and avoids missing opportunities to minimize the public surface area."
  , examples:
      { bad:
          [ """
module FlimFlam where

x :: Int 
x = 1
          """
          ]
      , good:
          [ """
module FlimFlam (x) where

x :: Int 
x = 1
          """
          ]
      }
  , lintProducer: const $ moduleLintProducer $ case _ of
      CST.Module { header: ModuleHeader { name: Name { token: { range: sourceRange } }, exports: Nothing } } ->
        [ { message: "Module should have explicit exports.", sourceRange } ]
      _ -> []
  }
