module Main where

import Prelude

import Control.Monad.Except (catchError, runExcept)
import Data.Array (filter, null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, filterWithKey, fromFoldable, isEmpty)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Foreign.Generic (defaultOptions, genericDecodeJSON)
import Milkis (URL(..), defaultFetchOptions, fetch, text)
import Milkis.Impl.Node (nodeFetch)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.Process (exit)
import Node.Yargs.Applicative (Y, runY, yarg)
import Node.Yargs.Setup (defaultHelp)
import Types (Dependencies(Dependencies), DependencyMap(DependencyMap), Entry, ElmJson(ElmJson), NewerDependencyMap(NewerDependencyMap), SearchJson, Version)

main :: Effect Unit
main = runY defaultHelp $ app <$> projectPathArgument
    where
        app :: String -> Effect Unit
        app projectPath =
            launchAff_ do
                dependencyMap <- getDepencencies projectPath

                liftEffect $ log "Fetching latest dependencies versions..."

                jsonSearch <- unwrap <$> fetchSearch

                dependencyMap
                    # mapWithIndex (findLaterVersions jsonSearch)
                        >>> fromFoldable
                        >>> filterWithKey (const $ not <<< null)
                        >>> logNewerDependencyMap
                        >>> liftEffect

        logNewerDependencyMap :: Map String (Array Version) -> Effect Unit
        logNewerDependencyMap m =
            if isEmpty m then
                log "Your dependencies are all up to date"
            else
                logShow $ NewerDependencyMap m

        findLaterVersions :: Array Entry -> String -> (Maybe Version) -> Tuple String (Array Version)
        findLaterVersions entries k v =
            Tuple k $ filter later allVersions
            where
                later :: Version -> Boolean
                later = Just >>> (<) v

                allVersions :: Array Version
                allVersions =
                    case filter (unwrap >>> _.name >>> (==) k) entries of
                        [m] -> unwrap m # _.versions
                        _ -> []

        projectPathArgument :: Y String
        projectPathArgument =
            yarg "project" ["p"] (Just "elm.json path") (Left "./elm.json") true

        fetchSearch :: Aff SearchJson
        fetchSearch =
            (attempt $ (fetch nodeFetch) (URL "https://package.elm-lang.org/search.json") defaultFetchOptions)
                >>= lmap (const unit) >>> traverse text
                >>= either (const $ cleanExit "An error occured while fetching the dependencies db") pure
                    <<< ((=<<) (genericDecodeJSON defaultOptions { unwrapSingleConstructors = true } >>> runExcept >>> lmap (const unit)))

        getDepencencies :: String -> Aff (Map String (Maybe Version))
        getDepencencies projectPath = do
            readTextFile UTF8 projectPath
                # (flip catchError $ const $ cleanExit ("Couldn't read file " <> projectPath))
                >>= \content ->
                    either
                        (const $ cleanExit "The provided elm.json file is invalid")
                        pure
                        $ (\(ElmJson { dependencies: Dependencies { direct: DependencyMap direct } }) -> direct)
                            <$> runExcept (genericDecodeJSON defaultOptions { unwrapSingleConstructors = true } content)

        cleanExit :: forall a. String -> Aff a
        cleanExit =
            (=<<) (const $ liftEffect $ exit 1) <<< log
