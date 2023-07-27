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
import Database.Postgres.PgCodec (RowCodec, PgCodec, fromPg, toPg, ParseErr)
import Database.Postgres.PgCodec as PgCodec
import Database.Postgres.PgCodec as K

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
          forall params. Eq params => Show params =>
          RowCodec params -> String -> String -> params
          -> SpecT Aff Pg.Connection Identity Unit
        itExpr codec pgType pgVal psVal = do
          it (pgVal <> " --parse-> " <>  show psVal) \conn -> do
            res <- conn # Pq.queryThrow_ codec ("SELECT " <> pgVal <> "::" <> pgType)
            res `shouldEqual` [psVal]
          it (pgVal <> " <-print-- " <> show psVal) \conn -> do
            res <- conn # Pq.queryThrow codec (PgCodec.row1 PgCodec.boolean) ("SELECT $1::" <> pgType <> " = " <> pgVal <> "::" <> pgType) psVal
            res `shouldEqual` [true]

      describe "Int" do
        itExpr (K.row1 K.int) "INT" "7" 7
        itExpr (K.row1 K.int) "INT" "-7" (-7)
        itExpr (K.row1 K.int) "INT" "0" 0

      describe "Number" do
        itExpr (K.row1 K.number) "REAL" "1.25" 1.25
        itExpr (K.row1 K.number) "REAL" "-1.25" (-1.25)
        itExpr (K.row1 K.number) "REAL" "0" 0.0

      describe "Maybe" do
        itExpr (K.row1 $ K.nullable K.number) "REAL" "NULL" (Nothing :: Maybe Number)
        itExpr (K.row1 $ K.nullable K.text) "TEXT" "'NULL'" $ Just "NULL"
        itExpr (K.row1 $ K.nullable K.text) "TEXT" "'null'" $ Just "null"

      describe "String" do
        itExpr (K.row1 K.text) "TEXT" "'abc'" "abc"
        itExpr (K.row1 K.text) "TEXT" "''" ""
        itExpr (K.row1 K.text) "TEXT" "'has \" quote'" "has \" quote"
        itExpr (K.row1 K.text) "TEXT" "'has '' quote'" "has ' quote"

      describe "Array" do
        itExpr (K.row1 $ K.arrayOf K.number) "REAL[]" "'{}'" ([] :: Array Number)
        itExpr (K.row1 $ K.arrayOf K.number) "REAL[]" "'{1,2,3}'" [1.0, 2.0, 3.0]
        itExpr (K.row1 $ K.arrayOf K.text) "TEXT[]" "'{a,\"\",c}'" ["a", "", "c"]
        itExpr (K.row1 $ K.arrayOf $ K.arrayOf K.int)  "INT[][]" "'{{1,2},{3,4},{5,6}}'" [[1, 2], [3, 4], [5, 6]]

      describe "Sets" do
        itExpr (K.row1 $ K.setOf K.number) "REAL[]" "'{}'" (Set.empty :: Set Number)
        itExpr (K.row1 $ K.setOf K.number) "REAL[]" "'{1,2,3}'" $ Set.fromFoldable [1.0, 2.0, 3.0]
        itExpr (K.row1 $ K.setOf K.text) "TEXT[]" "'{a,\"\",c}'" $ Set.fromFoldable ["a", "", "c"]
        itExpr (K.row1 $ K.setOf $ K.setOf K.int) "INT[][]" "'{{1,2},{3,4},{5,6}}'" $ Set.fromFoldable <<< map Set.fromFoldable $ [[1, 2], [3, 4], [5, 6]]

      describe "Tuple" do
        itExpr (K.row1 $ K.tup2 K.int K.int) "anyelement" "(10, 20)" (Pg.Tup $ 10 /\ 20)
        itExpr (K.row1 $ K.tup4 K.text K.text K.text K.text) "anyelement" "('', 'a''b', 'a\"b', NULL::TEXT)" (Pg.Tup $ "" /\ "a'b" /\ "a\"b" /\ "NULL")
        itExpr (K.row1 $ K.tup2 K.int (K.arrayOf K.int)) "anyelement" "(1, '{1, 2, 3}'::int[])" (Pg.Tup $ 1 /\ [1, 2, 3])
