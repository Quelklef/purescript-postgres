module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
import Data.Tuple.Nested ((/\))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set

import Database.Postgres.Connection (Connection, open) as Pg
import Database.Postgres.Types (Tup(..)) as Pg
import Database.Postgres.Query as Pq
import Database.Postgres.FromPg (class FromPg)
import Database.Postgres.ToPg (class ToPg)

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

    describe "single-expr printing/parsing" do

      let
        itExpr ::
          forall ps. Eq ps => Show ps => FromPg (Pg.Tup ps) => ToPg (Pg.Tup ps) =>
          String -> String -> ps -> SpecT Aff Pg.Connection Identity Unit
        itExpr pgType pgVal psVal = do
          it (pgVal <> " --parse-> " <>  show psVal) \conn -> do
            res <- conn # Pq.queryThrow_ ("SELECT " <> pgVal <> "::" <> pgType)
            res `shouldEqual` [Pg.Tup psVal]
          it (pgVal <> " <-print-- " <> show psVal) \conn -> do
            res <- conn # Pq.queryThrow ("SELECT $1::" <> pgType <> " = " <> pgVal <> "::" <> pgType) (Pg.Tup psVal)
            res `shouldEqual` [Pg.Tup true]

      describe "Int" do
        itExpr "INT" "7" 7
        itExpr "INT" "-7" (-7)
        itExpr "INT" "0" 0

      describe "Number" do
        itExpr "REAL" "1.25" 1.25
        itExpr "REAL" "-1.25" (-1.25)
        itExpr "REAL" "0" 0.0

      describe "Maybe" do
        itExpr "REAL" "NULL" (Nothing :: Maybe Number)
        itExpr "TEXT" "'NULL'" $ Just "NULL"
        itExpr "TEXT" "'null'" $ Just "null"

      describe "String" do
        itExpr "TEXT" "'abc'" "abc"
        itExpr "TEXT" "''" ""
        itExpr "TEXT" "'has \" quote'" "has \" quote"
        itExpr "TEXT" "'has '' quote'" "has ' quote"

      describe "Array" do
        itExpr "REAL[]" "'{}'" ([] :: Array Number)
        itExpr "REAL[]" "'{1,2,3}'" [1.0, 2.0, 3.0]
        itExpr "TEXT[]" "'{a,\"\",c}'" ["a", "", "c"]
        itExpr "INT[][]" "'{{1,2},{3,4},{5,6}}'" [[1, 2], [3, 4], [5, 6]]

      describe "Sets" do
        itExpr "REAL[]" "'{}'" (Set.empty :: Set Number)
        itExpr "REAL[]" "'{1,2,3}'" $ Set.fromFoldable [1.0, 2.0, 3.0]
        itExpr "TEXT[]" "'{a,\"\",c}'" $ Set.fromFoldable ["a", "", "c"]
        itExpr "INT[][]" "'{{1,2},{3,4},{5,6}}'" $ Set.fromFoldable <<< map Set.fromFoldable $ [[1, 2], [3, 4], [5, 6]]

      describe "Tuple" do
        itExpr "anyelement" "(10, 20)" (Pg.Tup $ 10 /\ 20)
        itExpr "anyelement" "('', 'a''b', 'a\"b', NULL::TEXT)" (Pg.Tup $ "" /\ "a'b" /\ "a\"b" /\ "NULL")
        itExpr "anyelement" "(1, '{1, 2, 3}'::int[])" (Pg.Tup $ 1 /\ [1, 2, 3])
