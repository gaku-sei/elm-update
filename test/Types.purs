module Test.Types (tests) where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Types (Version(Version))

buildVersion :: Int -> Int -> Int -> Version
buildVersion major minor patch = Version { major, minor, patch }

tests :: Spec Unit
tests =
  describe "Version Ordering" do
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
