module Linter.ModuleExports (exportsRequired) where

import Prelude

import Data.Maybe (Maybe(..))
import Linter (moduleLintProducer)
import Linter as Linter
import PureScript.CST.Types (ModuleHeader(..), Name(..))
import PureScript.CST.Types as CST

exportsRequired :: Linter.Linter
exportsRequired = Linter.mkWithNoConfig
  { name: "ModuleExportsRequired"
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
  , lintProducer: moduleLintProducer $ case _ of
      CST.Module { header: ModuleHeader { name: Name { token: { range: sourceRange } }, exports: Nothing } } ->
        [ { message: "Module should have explicit exports.", sourceRange } ]
      _ -> []
  }
