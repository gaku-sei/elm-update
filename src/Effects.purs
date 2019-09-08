module Effects (cleanExit, fetchSearch, getDepencencies, logNewerDependencyMap) where

import Prelude
import Control.Monad.Except (catchError, runExcept)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Map (Map, isEmpty)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Foreign.Generic (defaultOptions, genericDecodeJSON)
import Milkis (URL(URL), defaultFetchOptions, fetch, text)
import Milkis.Impl.Node (nodeFetch)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.Process (exit)
import Types
  ( Dependencies(Dependencies)
  , DependencyMap(DependencyMap)
  , ElmJson(ElmJson)
  , NewerDependencyMap(NewerDependencyMap)
  , SearchJson
  , Version
  )

cleanExit :: forall a. String -> Aff a
cleanExit = (=<<) (const $ liftEffect $ exit 1) <<< log

logNewerDependencyMap :: Map String (Array Version) -> Effect Unit
logNewerDependencyMap m
  | isEmpty m = log "Your dependencies are all up to date"
  | otherwise = logShow $ NewerDependencyMap m

fetchSearch :: Aff SearchJson
fetchSearch =
  (attempt $ (fetch nodeFetch) (URL "https://package.elm-lang.org/search.json") defaultFetchOptions)
    >>= lmap (const unit)
    >>> traverse text
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
