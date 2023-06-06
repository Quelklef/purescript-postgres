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
import Database.Postgres.ToPg (ToPg)
import Database.Postgres.ToPg as Tp

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
          forall ps. Eq ps => Show ps => FromPg (Pg.Tup ps) =>
          ToPg (Pg.Tup ps) -> String -> String -> ps -> SpecT Aff Pg.Connection Identity Unit
        itExpr toPg pgType pgVal psVal = do
          it (pgVal <> " --parse-> " <>  show psVal) \conn -> do
            res <- conn # Pq.queryThrow_ ("SELECT " <> pgVal <> "::" <> pgType)
            res `shouldEqual` [Pg.Tup psVal]
          it (pgVal <> " <-print-- " <> show psVal) \conn -> do
            res <- conn # Pq.queryThrow toPg ("SELECT $1::" <> pgType <> " = " <> pgVal <> "::" <> pgType) (Pg.Tup psVal)
            res `shouldEqual` [Pg.Tup true]

      describe "Int" do
        itExpr (Tp.toPg_Tup1 Tp.toPg_Int) "INT" "7" 7
        itExpr (Tp.toPg_Tup1 Tp.toPg_Int) "INT" "-7" (-7)
        itExpr (Tp.toPg_Tup1 Tp.toPg_Int) "INT" "0" 0

      describe "Number" do
        itExpr (Tp.toPg_Tup1 Tp.toPg_Number) "REAL" "1.25" 1.25
        itExpr (Tp.toPg_Tup1 Tp.toPg_Number) "REAL" "-1.25" (-1.25)
        itExpr (Tp.toPg_Tup1 Tp.toPg_Number) "REAL" "0" 0.0

      describe "Maybe" do
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Maybe Tp.toPg_Number) "REAL" "NULL" (Nothing :: Maybe Number)
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Maybe Tp.toPg_String) "TEXT" "'NULL'" $ Just "NULL"
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Maybe Tp.toPg_String) "TEXT" "'null'" $ Just "null"

      describe "String" do
        itExpr (Tp.toPg_Tup1 Tp.toPg_String) "TEXT" "'abc'" "abc"
        itExpr (Tp.toPg_Tup1 Tp.toPg_String) "TEXT" "''" ""
        itExpr (Tp.toPg_Tup1 Tp.toPg_String) "TEXT" "'has \" quote'" "has \" quote"
        itExpr (Tp.toPg_Tup1 Tp.toPg_String) "TEXT" "'has '' quote'" "has ' quote"

      describe "Array" do
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Array Tp.toPg_Number) "REAL[]" "'{}'" ([] :: Array Number)
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Array Tp.toPg_Number) "REAL[]" "'{1,2,3}'" [1.0, 2.0, 3.0]
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Array Tp.toPg_String) "TEXT[]" "'{a,\"\",c}'" ["a", "", "c"]
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Array $ Tp.toPg_Array Tp.toPg_Int) "INT[][]" "'{{1,2},{3,4},{5,6}}'" [[1, 2], [3, 4], [5, 6]]

      describe "Sets" do
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Set Tp.toPg_Number) "REAL[]" "'{}'" (Set.empty :: Set Number)
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Set Tp.toPg_Number) "REAL[]" "'{1,2,3}'" $ Set.fromFoldable [1.0, 2.0, 3.0]
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Set Tp.toPg_String) "TEXT[]" "'{a,\"\",c}'" $ Set.fromFoldable ["a", "", "c"]
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Set $ Tp.toPg_Set Tp.toPg_Int) "INT[][]" "'{{1,2},{3,4},{5,6}}'" $ Set.fromFoldable <<< map Set.fromFoldable $ [[1, 2], [3, 4], [5, 6]]

      describe "Tuple" do
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Tup2 Tp.toPg_Int Tp.toPg_Int) "anyelement" "(10, 20)" (Pg.Tup $ 10 /\ 20)
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Tup4 Tp.toPg_String Tp.toPg_String Tp.toPg_String Tp.toPg_String) "anyelement" "('', 'a''b', 'a\"b', NULL::TEXT)" (Pg.Tup $ "" /\ "a'b" /\ "a\"b" /\ "NULL")
        itExpr (Tp.toPg_Tup1 $ Tp.toPg_Tup2 Tp.toPg_Int (Tp.toPg_Array Tp.toPg_Int)) "anyelement" "(1, '{1, 2, 3}'::int[])" (Pg.Tup $ 1 /\ [1, 2, 3])
