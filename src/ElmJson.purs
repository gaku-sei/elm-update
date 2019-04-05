module ElmJson (ElmJson(..), Dependencies(..), Object(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map, insert)
import Data.Traversable (traverse)
import Foreign (F, Foreign, readString)
import Foreign.Class (class Decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Foreign.Internal (readObject)
import Foreign.Object (fold)

newtype Object = Object (Map String String)

derive instance repGenericObject :: Generic Object _

instance decodeObject :: Decode Object where
    decode :: Foreign -> F Object
    decode value =
        readObject value
            >>= traverse readString
            <#> fold (\acc k v -> insert k v acc) mempty
            <#> Object

newtype Dependencies = Dependencies
    { direct :: Object
    , indirect :: Object
    }

derive instance repGenericDependencies :: Generic Dependencies _

instance decodeDependencies :: Decode Dependencies where
    decode = genericDecode defaultOptions { unwrapSingleConstructors = true }

newtype ElmJson = ElmJson
    { dependencies :: Dependencies
    }

derive instance repGenericElmJson :: Generic ElmJson _

instance decodeElmJson :: Decode ElmJson where
    decode = genericDecode defaultOptions { unwrapSingleConstructors = true }
