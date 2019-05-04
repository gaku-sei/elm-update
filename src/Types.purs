module Types
  ( Dependencies(Dependencies)
  , DependencyMap(DependencyMap)
  , Entry
  , ElmJson(ElmJson)
  , NewerDependencyMap(NewerDependencyMap)
  , SearchJson
  , Version(Version)
  ) where

import Prelude

import Control.Monad.Except (except)
import Data.Array (fromFoldable, many, snoc)
import Data.Bifunctor (lmap)
import Data.Either (hush)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.List.Types (NonEmptyList(NonEmptyList))
import Data.Map (Map, insert)
import Data.Maybe (Maybe(Just), maybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Read (class Read, read)
import Data.Traversable (maximum, traverse)
import Foreign (ForeignError(ForeignError), readString)
import Foreign.Class (class Decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Foreign.Internal (readObject)
import Foreign.Object (fold)
import Text.Parsing.Parser (ParserT, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Token (digit)

-- The version record and its parser
-- Elm versions are always formatted like the following: major.minor.patch
data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  }

versionParser :: forall a. Monad a => ParserT String a Version
versionParser =
  (fromString <<< fromCharArray <$> many digit) `sepBy1` (string ".")
    <#> fromFoldable
    >>= case _ of
      [Just major, Just minor, Just patch] ->
          pure $ Version { major, minor, patch }

      _ ->
          fail "Version format is not valid"

derive instance genericVersion :: Generic Version _

instance decodeVersion :: Decode Version where
  decode value =
    readString value
      >>= (except
        <<< lmap (NonEmptyList <<< flip (:|) mempty <<< ForeignError <<< parseErrorMessage)
        <<< flip runParser versionParser)

-- Isomorphic show instance
instance showVersion :: Show Version where
  show (Version { major, minor, patch }) =
    (show major) <> "." <> (show minor) <> "." <> (show patch)

instance readVersion :: Read Version where
  read =
    hush <<< flip runParser versionParser

derive instance eqVersion :: Eq Version

-- Since the `Ord` generic derivation is based on the alphabetical order,
-- it means that renaming patch to, let's say, bugfixes, will completely
-- change the ordering of Version. Hence the manual instance.
instance ordVersion :: Ord Version where
  compare
    (Version { major, minor, patch })
    (Version { major: major', minor: minor', patch: patch' })
    | major > major' = GT
    | major < major' = LT
    | minor > minor' = GT
    | minor < minor' = LT
    | patch > patch' = GT
    | patch < patch' = LT
    | otherwise = EQ

-- The elm.json dependencies and their version parsed
newtype DependencyMap = DependencyMap (Map String (Maybe Version))

derive instance genericObject :: Generic DependencyMap _

instance decodeObject :: Decode DependencyMap where
  decode value =
    readObject value
      >>= traverse readString
      <#> fold (\acc k v -> insert k (read v) acc) mempty
      <#> DependencyMap

-- The elm.json direct dependencies
newtype Dependencies = Dependencies
  { direct :: DependencyMap
  }

derive instance genericDependencies :: Generic Dependencies _

instance decodeDependencies :: Decode Dependencies where
  decode =
    genericDecode defaultOptions { unwrapSingleConstructors = true }

-- The elm.json root object
newtype ElmJson = ElmJson
  { dependencies :: Dependencies
  }

derive instance genericElmJson :: Generic ElmJson _

instance decodeElmJson :: Decode ElmJson where
  decode =
    genericDecode defaultOptions { unwrapSingleConstructors = true }

-- The entry record fetched from the elm package search json
newtype Entry = Entry
  { name :: String
  , versions :: Array Version
  }

derive instance genericEntry :: Generic Entry _

derive instance newtypeEntry :: Newtype Entry _

instance decodeEntry :: Decode Entry where
  decode =
    genericDecode defaultOptions { unwrapSingleConstructors = true }

-- The root new type fetched from the elm package search json
newtype SearchJson = SearchJson (Array Entry)

derive instance genericSearchJson :: Generic SearchJson _

derive instance newtypeSearchJson :: Newtype SearchJson _

instance decodeSearchJson :: Decode SearchJson where
  decode =
    genericDecode defaultOptions { unwrapSingleConstructors = true }

-- This new type is used to isolate the pretty print logic for the dependencies
newtype NewerDependencyMap = NewerDependencyMap (Map String (Array Version))

derive instance newtypeNewerDependencyMap :: Newtype NewerDependencyMap _

instance showNewerDependencyMap :: Show NewerDependencyMap where
  show (NewerDependencyMap dependencyMap) =
    dependencyMap # foldrWithIndex format [] # joinWith "\n"
    where
      format :: String -> Array Version -> Array String -> Array String
      format k =
        flip snoc <<< (maybe "" ((<>) (k <> ": ") <<< show) <<< maximum)
