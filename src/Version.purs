module Version (Version(..)) where

import Prelude

import Control.Monad.Except (except)
import Data.Array (fromFoldable, many)
import Data.Bifunctor (lmap)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.String.CodeUnits (fromCharArray)
import Data.String.Read (class Read)
import Foreign (ForeignError(..), readString)
import Foreign.Class (class Decode)
import Text.Parsing.Parser (ParserT, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Token (digit)

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
    compare _ _ = LT
