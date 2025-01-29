module Main where

import Prelude

import CLI (cli)
import Effect (Effect)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = launchAff_ cli
