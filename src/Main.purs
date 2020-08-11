module Main where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (fold, many, some)
import Data.Char.Unicode (isAlphaNum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.String.CodeUnits (fromCharArray, singleton)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Global (readFloat)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, option, sepBy, try)
import Text.Parsing.Parser.String (class StringLike, char, eof, oneOf, satisfy, string, whiteSpace)

-- use eof for the end of a file when parsing files
parse_ba :: forall s. StringLike s => Parser s Char
parse_ba = oneOf [ 'a', 'b' ]

some_ba :: forall s. StringLike s => Parser s (Array Char)
some_ba = some parse_ba

sanityCheckSome :: forall s. StringLike s => Parser s String
sanityCheckSome = fromCharArray <$> some_ba

sanityCheckMany :: forall s. StringLike s => Parser s String
sanityCheckMany = fromCharArray <$> (many $ parse_ba)

parseNull :: forall s. StringLike s => Parser s Json
parseNull = string "null" *> pure JNull

parseBoolean :: forall s. StringLike s => Parser s Json
parseBoolean =
  (string "true" *> pure (JBoolean true))
    <|> (string "false" *> pure (JBoolean false))

negativeSign :: forall s. StringLike s => Parser s String
negativeSign = string "-"

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe a b = (+) <$> a <*> b

integerWhen0 :: forall s. StringLike s => Parser s String
integerWhen0 = append <$> (option "" negativeSign) <*> (string "0")

oneToNine = [ '1', '2', '3', '4', '5', '6', '7', '8', '9' ] :: Array Char

digits = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] :: Array Char

integerWhenNot0 :: forall s. StringLike s => Parser s String
integerWhenNot0 =
  fold
    <$> sequence
        [ (option "" negativeSign)
        , (singleton <$> oneOf oneToNine)
        , (fromCharArray <$> (many $ oneOf digits))
        ]

integerPart :: forall s. StringLike s => Parser s String
integerPart = (try integerWhen0) <|> integerWhenNot0

fractionalPart :: forall s. StringLike s => Parser s String
fractionalPart = append <$> (string ".") <*> (fromCharArray <$> (many $ oneOf digits))

floatWithFraction :: forall s. StringLike s => Parser s String
floatWithFraction = append <$> integerPart <*> fractionalPart

exponentPart :: forall s. StringLike s => Parser s String
exponentPart =
  fold
    <$> sequence
        [ (singleton <$> oneOf [ 'e', 'E' ])
        , (option "" (singleton <$> oneOf [ '+', '-' ]))
        , (fromCharArray <$> (many $ oneOf digits))
        ]

floatWithExponent :: forall s. StringLike s => Parser s String
floatWithExponent = append <$> integerPart <*> exponentPart

floatWithDigitAndExponent :: forall s. StringLike s => Parser s String
floatWithDigitAndExponent =
  fold
    <$> sequence
        [ integerPart
        , fractionalPart
        , exponentPart
        ]

parseNumber :: forall s. StringLike s => Parser s Json
parseNumber =
  (JNumber <<< readFloat)
    <$> ( (try floatWithFraction)
          <|> (try floatWithExponent)
          <|> (try floatWithDigitAndExponent)
          <|> integerPart
      )

parseString' :: forall s. StringLike s => Parser s String
parseString' =
  between
    (string "\"")
    (string "\"")
    (fromCharArray <$> many (satisfy isAlphaNum))

parseString :: forall s. StringLike s => Parser s Json
parseString = JString <$> parseString'

parseArray :: forall s. StringLike s => Parser s Json -> Parser s Json
parseArray pj =
  JArray
    <$> ( between
          (string "[" *> whiteSpace)
          (whiteSpace *> string "]")
          (sepBy (whiteSpace *> pj <* whiteSpace) (char ','))
      )

parseKeyValuePair :: forall s. StringLike s => Parser s Json -> Parser s (Tuple String Json)
parseKeyValuePair pj =
  Tuple
    <$> parseString'
    <*> (whiteSpace *> char ':' *> whiteSpace *> pj)

parseObject :: forall s. StringLike s => Parser s Json -> Parser s Json
parseObject pj =
  JObject
    <$> ( between
          (string "{" *> whiteSpace)
          (whiteSpace *> string "}")
          (sepBy (whiteSpace *> parseKeyValuePair pj <* whiteSpace) (char ','))
      )

parseJson :: forall s. StringLike s => Parser s Json
parseJson =
  fix \me ->
    (try (parseObject me))
      <|> (try (parseArray me))
      <|> (try parseNumber)
      <|> (try parseString)
      <|> (try parseBoolean)
      <|> (parseNull)

-- -3.0E-7
-- 4
-- 1.2
data Json
  = JNumber Number -- 1.0
  | JString String -- "hello world"
  | JNull -- null
  | JBoolean Boolean -- true
  | JArray (List Json) -- [ 1 , 2 ]
  | JObject (List (Tuple String Json)) -- {"a":"b","a":"c"}

derive instance genericJson :: Generic Json _

derive instance eqJson :: Eq Json

instance jsonShow :: Show Json where
  show a = genericShow a
