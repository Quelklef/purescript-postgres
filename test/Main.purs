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
import Database.Postgres.Types (Tup(..), tup0) as Pg
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
        itParses ::
          forall params. Eq params => Show params =>
          RowCodec params -> String -> String -> params
          -> SpecT Aff Pg.Connection Identity Unit
        itParses codec pgType pgVal psVal = do
          it (pgVal <> " --parse-> " <>  show psVal) \conn -> do
            res <- conn # Pq.queryThrow_ codec ("SELECT " <> pgVal <> "::" <> pgType)
            res `shouldEqual` [psVal]
        itPrints ::
          forall params. Eq params => Show params =>
          RowCodec params -> String -> String -> params
          -> SpecT Aff Pg.Connection Identity Unit
        itPrints codec pgType pgVal psVal = do
          it (pgVal <> " <-print-- " <> show psVal) \conn -> do
            res <- conn # Pq.queryThrow codec (PgCodec.row1 PgCodec.boolean) ("SELECT $1::" <> pgType <> " IS NOT DISTINCT FROM " <> pgVal <> "::" <> pgType) psVal
            res `shouldEqual` [true]
        itParsesAndPrints ::
          forall params. Eq params => Show params =>
          RowCodec params -> String -> String -> params
          -> SpecT Aff Pg.Connection Identity Unit
        itParsesAndPrints codec pgType pgVal psVal = do
          itParses codec pgType pgVal psVal
          itPrints codec pgType pgVal psVal

      describe "Int" do
        itParsesAndPrints (K.row1 K.int) "INT" "7" 7
        itParsesAndPrints (K.row1 K.int) "INT" "-7" (-7)
        itParsesAndPrints (K.row1 K.int) "INT" "0" 0

      describe "Number" do
        itParsesAndPrints (K.row1 K.number) "REAL" "1.25" 1.25
        itParsesAndPrints (K.row1 K.number) "REAL" "-1.25" (-1.25)
        itParsesAndPrints (K.row1 K.number) "REAL" "0" 0.0

      describe "Maybe" do
        itParsesAndPrints (K.row1 $ K.nullable K.number) "REAL" "NULL" (Nothing :: Maybe Number)
        itParsesAndPrints (K.row1 $ K.nullable K.text) "TEXT" "'NULL'" $ Just "NULL"
        itParsesAndPrints (K.row1 $ K.nullable K.text) "TEXT" "'null'" $ Just "null"

      describe "String" do
        itParsesAndPrints (K.row1 K.text) "TEXT" "'abc'" "abc"
        itParsesAndPrints (K.row1 K.text) "TEXT" "''" ""
        itParsesAndPrints (K.row1 K.text) "TEXT" "'has \" quote'" "has \" quote"
        itParsesAndPrints (K.row1 K.text) "TEXT" "'has '' quote'" "has ' quote"

      describe "Array" do
        itParsesAndPrints (K.row1 $ K.arrayOf K.number) "REAL[]" "'{}'" ([] :: Array Number)
        itParsesAndPrints (K.row1 $ K.arrayOf K.number) "REAL[]" "'{1,2,3}'" [1.0, 2.0, 3.0]
        itParsesAndPrints (K.row1 $ K.arrayOf K.text) "TEXT[]" "'{a,\"\",c}'" ["a", "", "c"]
        itParsesAndPrints (K.row1 $ K.arrayOf $ K.nullable K.text) "TEXT[]" "'{a,null,c}'" [Just "a", Nothing, Just "c"]
        -- non-priority, not used in ⅄, annoying to implement
        --itParsesAndPrints (K.row1 $ K.arrayOf $ K.arrayOf K.int)  "INT[][]" "'{{1,2},{3,4},{5,6}}'" [[1, 2], [3, 4], [5, 6]]
        itParsesAndPrints (K.row1 $ K.arrayOf K.text) "TEXT[]" """'{"",a''b,"a\"b","NULL"}'""" ["", "a'b", "a\"b", "NULL"]
        itParsesAndPrints (K.row1 $ K.arrayOf K.text) "TEXT[]" """'{"a\\b"}'""" ["a\\b"]

      describe "Sets" do
        itParsesAndPrints (K.row1 $ K.setOf K.number) "REAL[]" "'{}'" (Set.empty :: Set Number)
        itParsesAndPrints (K.row1 $ K.setOf K.number) "REAL[]" "'{1,2,3}'" $ Set.fromFoldable [1.0, 2.0, 3.0]
        -- TODO don't sort sets
        itParsesAndPrints (K.row1 $ K.setOf K.text) "TEXT[]" "'{\"\",a,c}'" $ Set.fromFoldable ["a", "", "c"]
        -- non-priority, not used in ⅄, annoying to implement
        --itParsesAndPrints (K.row1 $ K.setOf $ K.setOf K.int) "INT[][]" "'{{1,2},{3,4},{5,6}}'" $ Set.fromFoldable <<< map Set.fromFoldable $ [[1, 2], [3, 4], [5, 6]]

      describe "Tuple" do
        -- postgres itself doesn't support the parsing of anonymous composite types
        -- `ERROR:  input of anonymous composite types is not implemented`
        -- ⅄ only needs parsing
        itParses (K.row1 $ K.tup2 K.int K.int) "anyelement" "(10, 20)" (Pg.Tup $ 10 /\ 20)
        itParses (K.row1 $ K.tup4 K.text K.text K.text (K.nullable K.text)) "anyelement" "('', 'a''b', 'a\"b', NULL::TEXT)" (Pg.Tup $ "" /\ "a'b" /\ "a\"b" /\ Nothing)
        itParses (K.row1 $ K.tup2 K.int (K.arrayOf K.int)) "anyelement" "(1, '{1, 2, 3}'::int[])" (Pg.Tup $ 1 /\ [1, 2, 3])
        itParses (K.row1 $ K.tup2 K.int (K.nullable K.int)) "anyelement" "(1,null)" (Pg.Tup $ 1 /\ Nothing)
        itParses (K.row1 $ K.tup1 K.text) "anyelement" """row('"),)')""" (Pg.Tup $ "\"),)")
        itParses (K.row1 $ K.tup4 K.text K.text K.text K.text) "anyelement" """('hello','"','),)','')""" (Pg.Tup $ "hello" /\ "\"" /\ "),)" /\ "")

        itParses (K.row1 $ K.tup0) "anyelement" """row()""" (Pg.tup0)
        itParses (K.row1 $ K.tup1 $ K.nullable K.text) "anyelement" """row(null)""" (Pg.Tup $ Nothing)
