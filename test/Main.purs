module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)

import Database.Postgres.Connection (Connection, open) as Pg
import Database.Postgres.Query as Pq
import Database.Postgres.FromPg (class FromPg)

import Test.Spec (Spec, SpecT, around, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)

foreign import getConnStr :: Effect String

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] $ spec

withDb :: (Pg.Connection -> Aff Unit) -> Aff Unit
withDb action =
  do
    connStr <- liftEffect getConnStr
    conn <- Pg.open connStr
    action conn

spec :: Spec Unit
spec = around withDb $ do
  describe "purescript-postgres" do
    describe "pg expression parsing" do

      let
        testParse :: forall r. Show r => Eq r => FromPg r => Pg.Connection -> String -> Array r -> Aff Unit
        testParse conn expr expected = do
          res <- conn # Pq.queryThrow_ ("SELECT " <> expr)
          res `shouldEqual` expected

      it "should parse INT" \conn -> do
        testParse conn "7::INT" [7]

      it "should parse REAL" \conn -> do
        testParse conn "1.25::REAL" [1.25]
