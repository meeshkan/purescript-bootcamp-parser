module Test.Main where

import Prelude
import Data.Either (Either(..), isLeft)
import Data.List (List(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (Json(..), floatWithDigitAndExponent, floatWithExponent, floatWithFraction, integerPart, integerWhen0, integerWhenNot0, parseBoolean, parseJson, parseNull, parseNumber, parseString, sanityCheckMany, sanityCheckSome)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parsing.Parser (runParser)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "dummy parser" do
          it "parses some b's and a's" do
            Right "aaaaa" `shouldEqual` (runParser "aaaaa" sanityCheckSome)
            Right "aaaaaaaaaaaa" `shouldEqual` (runParser "aaaaaaaaaaaa" sanityCheckSome)
            true `shouldEqual` isLeft (runParser "caaaaaaaaaaaa" sanityCheckSome)
            Right "" `shouldEqual` (runParser "caaaaaaaaaaaa" sanityCheckMany)
            Right "abba" `shouldEqual` (runParser "abba" sanityCheckSome)
            Right "aaaaa" `shouldEqual` (runParser "aaaaaca" sanityCheckSome)
        describe "json primitives" do
          it "should parse an 0 when integer" do
            Right "0" `shouldEqual` (runParser "0" integerWhen0)
            Right "-0" `shouldEqual` (runParser "-0" integerWhen0)
            true `shouldEqual` isLeft (runParser "1" integerWhen0)
          it "should parse a non-zero integer" do
            Right "42" `shouldEqual` (runParser "42" integerWhenNot0)
            Right "1" `shouldEqual` (runParser "1" integerWhenNot0)
            Right "-1" `shouldEqual` (runParser "-1" integerWhenNot0)
            true `shouldEqual` isLeft (runParser "0" integerWhenNot0)
          it "should parse an integer part" do
            Right "42" `shouldEqual` (runParser "42" integerPart)
            Right "0" `shouldEqual` (runParser "0" integerPart)
            Right "-42" `shouldEqual` (runParser "-42" integerPart)
          it "should parse a simple float" do
            Right "3.1416" `shouldEqual` (runParser "3.1416" floatWithFraction)
            Right "-3.1416" `shouldEqual` (runParser "-3.1416" floatWithFraction)
          it "should parse an exponent float" do
            Right "3e10" `shouldEqual` (runParser "3e10" floatWithExponent)
            Right "-3E-4" `shouldEqual` (runParser "-3E-4" floatWithExponent)
            Right "-3E+4" `shouldEqual` (runParser "-3E+4" floatWithExponent)
          it "should parse an exponent float with digits" do
            Right "3.1e10" `shouldEqual` (runParser "3.1e10" floatWithDigitAndExponent)
            Right "-3.1E-4" `shouldEqual` (runParser "-3.1E-4" floatWithDigitAndExponent)
            Right "-3.1E+4" `shouldEqual` (runParser "-3.1E+4" floatWithDigitAndExponent)
        describe "json parser" do
          it "should parse null" do
            Right JNull `shouldEqual` (runParser "null" parseNull)
            Right JNull `shouldEqual` (runParser "null" parseJson)
            true `shouldEqual` isLeft (runParser "foo" parseNull)
          it "should parse boolean" do
            Right (JBoolean true) `shouldEqual` (runParser "true" parseBoolean)
            Right (JBoolean false) `shouldEqual` (runParser "false" parseBoolean)
            true `shouldEqual` isLeft (runParser "foo" parseBoolean)
          it "should parse a (admittedly simple) string" do
            Right (JString "hello") `shouldEqual` (runParser "\"hello\"" parseString)
            true `shouldEqual` isLeft (runParser "foo" parseString)
          it "should parse number" do
            Right (JNumber 3.1416) `shouldEqual` (runParser "3.1416" parseNumber)
            Right (JNumber 3.0) `shouldEqual` (runParser "3" parseNumber)
            Right (JNumber (negate 3.1416)) `shouldEqual` (runParser "-3.1416" parseNumber)
          it "should parse an array" do
            Right (JArray Nil) `shouldEqual` (runParser "[]" parseJson)
            Right (JArray (Cons (JNumber 1.0) Nil)) `shouldEqual` (runParser "[1]" parseJson)
            Right (JArray (Cons (JNumber 1.0) Nil)) `shouldEqual` (runParser "[1 ]" parseJson)
            Right (JArray (Cons (JNumber 1.0) Nil)) `shouldEqual` (runParser "[ 1]" parseJson)
            Right (JArray (Cons (JNumber 1.0) Nil)) `shouldEqual` (runParser "[ 1 ]" parseJson)
            true `shouldEqual` isLeft (runParser "[foo]" parseJson)
          it "should parse an object" do
            Right (JObject Nil) `shouldEqual` (runParser "{}" parseJson)
            Right (JObject (Cons (Tuple "a" (JNumber 1.0)) Nil)) `shouldEqual` (runParser "{\"a\":1}" parseJson)
            Right (JObject (Cons (Tuple "a" (JNumber 1.0)) Nil)) `shouldEqual` (runParser "{ \"a\":1}" parseJson)
            Right (JObject (Cons (Tuple "a" (JNumber 1.0)) Nil)) `shouldEqual` (runParser "{ \"a\" :1}" parseJson)
            Right (JObject (Cons (Tuple "a" (JNumber 1.0)) Nil)) `shouldEqual` (runParser "{ \"a\" :   1}" parseJson)
            Right (JObject (Cons (Tuple "a" (JNumber 1.0)) Nil)) `shouldEqual` (runParser "{ \"a\" :   1    }" parseJson)
            Right
              ( JObject
                  ( Cons (Tuple "a" (JNumber 1.0))
                      (Cons (Tuple "b" (JNumber 2.0)) Nil)
                  )
              )
              `shouldEqual`
                (runParser "{ \"a\" :   1  , \"b\": 2.0 }" parseJson)
            true `shouldEqual` isLeft (runParser "[foo]" parseJson)
