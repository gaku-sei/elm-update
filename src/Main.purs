module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (any, filter)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Exception (throw)
import ElmJson (ElmJson(ElmJson), Dependencies(Dependencies), DependencyMap(DependencyMap))
import Foreign.Generic (defaultOptions, genericDecodeJSON)
import Milkis (URL(..), defaultFetchOptions, fetch, text)
import Milkis.Impl.Node (nodeFetch)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.Yargs.Applicative (Y, runY, yarg)
import Node.Yargs.Setup (defaultHelp)
import SearchJson (Entry, SearchJson)
import Version (Version)

main :: Effect Unit
main = runY defaultHelp $ app <$> projectPathArgument
    where
        app :: String -> Effect Unit
        app projectPath =
            launchAff_ do
                jsonSearch <- fetchSearch <#> unwrap
                dependencies <- getDepencencies projectPath

                liftEffect $ logShow $ fromFoldable $ findUpper jsonSearch `mapWithIndex` dependencies

                -- liftEffect $ logShow jsonSearch

                -- liftEffect $ logShow dependencies

        findUpper :: Array Entry -> String -> (Maybe Version) -> Tuple String Boolean
        findUpper entries k v =
            Tuple k $ (Just >>> (>) v) `any` (
                case filter (unwrap >>> _.name >>> (==) k) entries of
                        [m] -> unwrap m # _.versions
                        _ -> [])

        projectPathArgument :: Y String
        projectPathArgument =
            yarg "project" ["p"] (Just "elm.json path") (Left "./elm.json") true

        fetchSearch :: Aff SearchJson
        fetchSearch =
            (attempt $ (fetch nodeFetch) (URL "https://package.elm-lang.org/search.json") defaultFetchOptions)
                >>= lmap (const unit) >>> traverse text
                >>= either (const $ liftEffect $ throw "An error occured when fetching the dependencies db") pure
                    <<< ((=<<) (genericDecodeJSON defaultOptions { unwrapSingleConstructors = true } >>> runExcept >>> lmap (const unit)))

        getDepencencies :: String -> Aff (Map String (Maybe Version))
        getDepencencies projectPath = do
            readTextFile UTF8 projectPath
                >>= \content ->
                    pure $ fold
                        $ (\(ElmJson { dependencies: Dependencies { direct: DependencyMap direct } }) -> direct)
                        <$> runExcept (genericDecodeJSON defaultOptions { unwrapSingleConstructors = true } content)
