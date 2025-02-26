module Test.Main where

import Prelude

import Effect (Effect)
import Test.CST as CST
import Test.Rules as Rules
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  CST.cst
  Rules.main
