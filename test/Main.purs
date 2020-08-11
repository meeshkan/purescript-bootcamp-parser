module Test.Main where

import Prelude
import Data.Either (Either(..), isLeft)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (sanityCheckMany, sanityCheckSome)
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
