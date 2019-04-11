module SearchJson where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Version (Version)

newtype Entry = Entry
    { name :: String
    , versions :: Array Version
    }

derive instance repGenericEntry :: Generic Entry _

derive instance newtypeEntry :: Newtype Entry _

derive newtype instance showEntry :: Show Entry

instance decodeEntry :: Decode Entry where
    decode =
        genericDecode defaultOptions { unwrapSingleConstructors = true }


newtype SearchJson = SearchJson (Array Entry)

derive instance repGenericSearchJson :: Generic SearchJson _

derive instance newtypeSearchJson :: Newtype SearchJson _

derive newtype instance showSearchJson :: Show SearchJson

instance decodeSearchJson :: Decode SearchJson where
    decode =
        genericDecode defaultOptions { unwrapSingleConstructors = true }
