module Test.Common where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (Module)
import Test.Spec.Assertions (fail)

simpleModulePrefix :: String
simpleModulePrefix = "module X where\n\n"

assertCode :: forall m. MonadThrow Error m => String -> (Module Void -> m Unit) -> m Unit
assertCode code test =
  case (parseModule code) of
    ParseSucceeded m -> test m
    ParseSucceededWithErrors _ _ -> fail "Failed to parse"
    ParseFailed _ -> fail "Failed to parse"