module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import ElmJson (ElmJson(ElmJson), Dependencies(Dependencies), Object(Object))
import Foreign.Generic (defaultOptions, genericDecodeJSON)
import Milkis (URL(..), defaultFetchOptions, fetch, text)
import Milkis.Impl.Node (nodeFetch)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)

logSearch :: Aff (Either Error Unit)
logSearch =
    (attempt $ (fetch nodeFetch) (URL "https://package.elm-lang.org/search.json") defaultFetchOptions)
        >>= (traverse $ text >=> log)

main :: Effect Unit
main = do
    _ <- launchAff do
        _ <- logSearch

        content <- readTextFile UTF8 "./assets/elm.json"

        liftEffect $ logShow $ fold $
            (\(ElmJson
                { dependencies: Dependencies
                    { direct: Object direct
                    , indirect: Object indirect
                    }
                }
            ) -> direct <> indirect) <$>
            runExcept (genericDecodeJSON defaultOptions { unwrapSingleConstructors = true } content :: _ ElmJson)

    pure unit
