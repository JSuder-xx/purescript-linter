module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.CST as CST
import Test.Linter as Linters
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  CST.cst
  Linters.linters
