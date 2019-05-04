module Test.Types (tests) where

import Prelude

import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Effect.Aff (Aff)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Types (Version(Version))

buildVersion :: Int -> Int -> Int -> Version
buildVersion major minor patch =
  Version { major, minor, patch }

tests :: StateT (Array (Group (Aff Unit))) Identity Unit
tests =
  describe "Version" do
    ord

ord :: StateT (Array (Group (Aff Unit))) Identity Unit
ord =
  describe "ordVersion" do
    it "properly compare versions" do
      buildVersion 1 0 0 `compare` buildVersion 1 0 0 `shouldEqual` EQ
      buildVersion 0 0 0 `compare` buildVersion 0 0 0 `shouldEqual` EQ
      buildVersion 0 2 0 `compare` buildVersion 0 2 0 `shouldEqual` EQ
      buildVersion 0 0 3 `compare` buildVersion 0 0 3 `shouldEqual` EQ
      buildVersion 1 2 3 `compare` buildVersion 1 2 3 `shouldEqual` EQ
      buildVersion 1 0 0 `compare` buildVersion 0 0 0 `shouldEqual` GT
      buildVersion 1 0 0 `compare` buildVersion 0 1 0 `shouldEqual` GT
      buildVersion 1 0 0 `compare` buildVersion 0 0 1 `shouldEqual` GT
      buildVersion 1 0 0 `compare` buildVersion 0 1 1 `shouldEqual` GT
      buildVersion 1 0 0 `compare` buildVersion 2 0 0 `shouldEqual` LT
      buildVersion 1 1 0 `compare` buildVersion 2 0 0 `shouldEqual` LT
      buildVersion 1 0 1 `compare` buildVersion 2 0 0 `shouldEqual` LT
