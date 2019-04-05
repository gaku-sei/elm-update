module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Generic (defaultOptions, genericDecodeJSON)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)

newtype ElmJson = ElmJson
    { type :: String
    }

derive instance repGenericElmJson :: Generic ElmJson _

main :: Effect Unit
main = do
    _ <- launchAff do
        content <- readTextFile UTF8 "./assets/elm.json"
        let type'' = fold $
            (\(ElmJson { type: type' }) -> type') <$>
            runExcept (genericDecodeJSON defaultOptions { unwrapSingleConstructors = true } content :: _ ElmJson)
        liftEffect $ log type''
        pure unit

    pure unit
