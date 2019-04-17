module Types
    ( Dependencies(Dependencies)
    , DependencyMap(DependencyMap)
    , Entry
    , ElmJson(ElmJson)
    , Packages(Packages)
    , SearchJson
    , Version(Version)
    ) where

import Prelude

import Control.Monad.Except (except)
import Data.Array (fromFoldable, many)
import Data.Bifunctor (lmap)
import Data.Either (hush)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.List.Types (NonEmptyList(..))
import Data.Map (Map, insert)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.String.CodeUnits (fromCharArray)
import Data.String.Read (class Read, read)
import Data.Traversable (maximum, traverse)
import Foreign (ForeignError(..), readString)
import Foreign.Class (class Decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Foreign.Internal (readObject)
import Foreign.Object (fold)
import Text.Parsing.Parser (ParserT, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Token (digit)

-- The version object and its parser
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

derive instance repGenericVersion :: Generic Version _

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
-- it means that renaming patch, to let's say, bugfixes, will completely
-- change the ordering of Version. Hence, the manual instance.
instance ordVersion :: Ord Version where
    compare (Version { major }) (Version { major: major' })
        | major > major' = GT
        | major < major' = LT
    compare (Version { minor }) (Version { minor: minor' })
        | minor > minor' = GT
        | minor < minor' = LT
    compare (Version { patch }) (Version { patch: patch' })
        | patch > patch' = GT
        | patch < patch' = LT
    compare (Version { major, minor, patch }) (Version { major: major', minor: minor', patch: patch' })
        | major == major' && minor == minor' && patch == patch' = EQ
    -- This one should not even be reachable
    compare _ _ = EQ


-- The elm.json dependencies and their version parsed
newtype DependencyMap = DependencyMap (Map String (Maybe Version))

derive instance repGenericObject :: Generic DependencyMap _

derive newtype instance showDependencyMap :: Show DependencyMap

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

derive instance repGenericDependencies :: Generic Dependencies _

derive newtype instance showDependencies :: Show Dependencies

instance decodeDependencies :: Decode Dependencies where
    decode =
        genericDecode defaultOptions { unwrapSingleConstructors = true }

-- The elm.json root object
newtype ElmJson = ElmJson
    { dependencies :: Dependencies
    }

derive instance repGenericElmJson :: Generic ElmJson _

derive newtype instance showElmJson :: Show ElmJson

instance decodeElmJson :: Decode ElmJson where
    decode =
        genericDecode defaultOptions { unwrapSingleConstructors = true }

-- The entry object fetched from the elm package search json
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

-- The root object fetched from the elm package search json
newtype SearchJson = SearchJson (Array Entry)

derive instance repGenericSearchJson :: Generic SearchJson _

derive instance newtypeSearchJson :: Newtype SearchJson _

derive newtype instance showSearchJson :: Show SearchJson

instance decodeSearchJson :: Decode SearchJson where
    decode =
        genericDecode defaultOptions { unwrapSingleConstructors = true }

-- This object is only a simple new type used to isolate the pretty print logic for the dependencies
-- TODO: Find a better naming
newtype Packages = Packages (Map String (Array Version))

derive instance newtypePackages :: Newtype Packages _

instance showPackages :: Show Packages where
    show packages =
        foldMapWithIndex (\k -> maybe "" (\v -> k <> ": " <> show v <> "\n") <<< maximum)
            $ unwrap packages
