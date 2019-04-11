module ElmJson (ElmJson(..), Dependencies(..), DependencyMap(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map, insert)
import Data.Maybe (Maybe)
import Data.String.Read (read)
import Data.Traversable (traverse)
import Foreign (readString)
import Foreign.Class (class Decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Foreign.Internal (readObject)
import Foreign.Object (fold)
import Version (Version)

newtype DependencyMap = DependencyMap (Map String (Maybe Version))

derive instance repGenericObject :: Generic DependencyMap _

derive newtype instance showDependencyMap :: Show DependencyMap

instance decodeObject :: Decode DependencyMap where
    decode value =
        readObject value
            >>= traverse readString
            <#> fold (\acc k v -> insert k (read v) acc) mempty
            <#> DependencyMap

newtype Dependencies = Dependencies
    { direct :: DependencyMap
    }

derive instance repGenericDependencies :: Generic Dependencies _

derive newtype instance showDependencies :: Show Dependencies

instance decodeDependencies :: Decode Dependencies where
    decode =
        genericDecode defaultOptions { unwrapSingleConstructors = true }

newtype ElmJson = ElmJson
    { dependencies :: Dependencies
    }

derive instance repGenericElmJson :: Generic ElmJson _

derive newtype instance showElmJson :: Show ElmJson

instance decodeElmJson :: Decode ElmJson where
    decode =
        genericDecode defaultOptions { unwrapSingleConstructors = true }
