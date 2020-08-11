module Main where

import Prelude
import Data.Array (many, some)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (class StringLike, oneOf)

parse_ba :: forall s. StringLike s => Parser s Char
parse_ba = oneOf [ 'a', 'b' ]

some_ba :: forall s. StringLike s => Parser s (Array Char)
some_ba = some parse_ba

sanityCheckSome :: forall s. StringLike s => Parser s String
sanityCheckSome = fromCharArray <$> some_ba

sanityCheckMany :: forall s. StringLike s => Parser s String
sanityCheckMany = fromCharArray <$> (many $ parse_ba)

main :: Effect Unit
main = do
  log "🍝"
