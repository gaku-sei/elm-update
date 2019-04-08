module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Test.Version as Version

main :: Effect Unit
main =
    run [consoleReporter] do
        Version.tests
