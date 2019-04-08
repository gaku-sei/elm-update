module SearchJson where

import Data.Generic.Rep (class Generic)
import Foreign.Class (class Decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Version (Version)

newtype Entry = Entry
    { name :: String
    , versions :: Array Version
    }

derive instance repGenericEntry :: Generic Entry _

instance decodeEntry :: Decode Entry where
    decode =
        genericDecode defaultOptions { unwrapSingleConstructors = true }


newtype SearchJson = SearchJson (Array Entry)

derive instance repGenericSearchJson :: Generic SearchJson _

instance decodeSearchJson :: Decode SearchJson where
    decode =
        genericDecode defaultOptions { unwrapSingleConstructors = true }
