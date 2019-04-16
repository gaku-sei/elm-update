module Packages (Packages(..)) where

import Prelude

import Data.Foldable (maximum)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map (Map)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap)
import Version (Version)

newtype Packages = Packages (Map String (Array Version))

derive instance newtypePackages :: Newtype Packages _

instance showPackages :: Show Packages where
    show packages =
        foldMapWithIndex (\k -> maybe "" (\v -> k <> ": " <> show v <> "\n") <<< maximum)
            $ unwrap packages
