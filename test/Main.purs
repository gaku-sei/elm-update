module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Types as Types

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        Types.tests
