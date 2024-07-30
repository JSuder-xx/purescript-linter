module Test.Common where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (Module)
import Test.Spec.Assertions (fail)

prefix :: String
prefix = "module X where\n\n"

assertCode :: forall m10. MonadThrow Error m10 => String -> (Module Void -> m10 Unit) -> m10 Unit
assertCode code test =
  case (parseModule $ prefix <> code) of
    ParseSucceeded m -> test m
    ParseSucceededWithErrors _ _ -> fail "Failed to parse"
    ParseFailed _ -> fail "Failed to parse"