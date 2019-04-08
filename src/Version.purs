module Version (Version) where

import Prelude

import Data.Array (fromFoldable, many)
import Data.Either (hush)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.String.Read (class Read)
import Text.Parsing.Parser (ParserT, fail, runParser)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Token (digit)

data Version = Version
    { major :: Int
    , minor :: Int
    , patch :: Int
    }

instance showVersion :: Show Version where
    show (Version { major, minor, patch }) =
        (show major) <> "." <> (show minor) <> "." <> (show patch)

instance readVersion :: Read Version where
    read =
        hush <<< flip runParser versionParser
        where
            versionParser :: forall a. Monad a => ParserT String a Version
            versionParser =
                (fromString <<< fromCharArray <$> many digit) `sepBy1` (string ".") <#>
                    fromFoldable >>=
                    case _ of
                        [Just major, Just minor, Just patch] ->
                            pure $ Version { major, minor, patch }

                        _ ->
                            fail "Version format is not valid"
