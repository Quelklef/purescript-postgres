module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Data.Unit (Unit)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] $ spec

spec :: Spec Unit
spec = do
  describe "purescript-postgres" do
    it "should have tests that run" do
      true `shouldEqual` true
