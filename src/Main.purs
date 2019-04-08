module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import ElmJson (ElmJson(ElmJson), Dependencies(Dependencies), DependencyMap(DependencyMap))
import Foreign.Generic (defaultOptions, genericDecodeJSON)
import Milkis (URL(..), defaultFetchOptions, fetch, text)
import Milkis.Impl.Node (nodeFetch)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import SearchJson (SearchJson)
import Version (Version)

main :: Effect Unit
main =
    launchAff_ do
        -- _ <- fetchSearch

        content <- readTextFile UTF8 "./assets/elm.json"

        liftEffect $ logShow $ fold $ concatDependencies
            <$> runExcept (genericDecodeJSON defaultOptions { unwrapSingleConstructors = true } content :: _ ElmJson)

    where
        fetchSearch :: Aff (Either Unit SearchJson)
        fetchSearch =
            (attempt $ (fetch nodeFetch) (URL "https://package.elm-lang.org/search.json") defaultFetchOptions)
                >>= (lmap (const unit) >>> traverse text)
                <#> (=<<) ((lmap $ const unit) <<< runExcept <<< genericDecodeJSON defaultOptions { unwrapSingleConstructors = true })

        concatDependencies :: ElmJson -> Map String (Maybe Version)
        concatDependencies (ElmJson { dependencies: Dependencies { direct: DependencyMap direct , indirect: DependencyMap indirect } }) =
            direct <> indirect
