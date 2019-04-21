module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Test.Types as Types

main :: Effect Unit
main =
    run [consoleReporter] do
        Types.tests
