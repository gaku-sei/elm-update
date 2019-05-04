module Main where

import Prelude

import Data.Array (filter, null)
import Data.Either (Either(Left))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (filterWithKey, fromFoldable)
import Data.Maybe (Maybe(Just))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effects (fetchSearch, getDepencencies, logNewerDependencyMap)
import Node.Yargs.Applicative (Y, runY, yarg)
import Node.Yargs.Setup (defaultHelp)
import Types (Entry, Version)

main :: Effect Unit
main =
  runY defaultHelp $ app <$> projectPathArgument
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

    findLaterVersions :: Array Entry -> String -> (Maybe Version) -> Tuple String (Array Version)
    findLaterVersions entries k v =
      Tuple k $ filter (Just >>> (<) v) allVersions
      where
        allVersions :: Array Version
        allVersions =
          case filter (unwrap >>> _.name >>> (==) k) entries of
            [m] -> unwrap m # _.versions
            _ -> []

    projectPathArgument :: Y String
    projectPathArgument =
      yarg "project" ["p"] (Just "elm.json path") (Left "./elm.json") true
