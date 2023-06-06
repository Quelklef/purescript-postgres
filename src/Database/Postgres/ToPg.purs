module Database.Postgres.ToPg
  -- ( class ToPg
  ( ToPg
  -- , toPg

  -- v Forced exports
  -- , class InnerTup
  -- , toPg_inner
  , toPg_Int
  , toPg_Number
  , toPg_String
  , toPg_Maybe
  , toPg_Array
  , toPg_Set
  , toPg_Tup0
  , toPg_Tup1
  , toPg_Tup2
  , toPg_Tup3
  , toPg_Tup4
  ) where

import Prelude

import Data.Foldable (intercalate, foldr)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Array as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.String.Common (replaceAll) as Str
import Data.String.Pattern (Pattern(..), Replacement(..)) as Str
import Data.Newtype (un)

import Database.Postgres.Types (Tup(..), PgExpr(..))

replace :: { this :: String, with :: String } -> String -> String
replace { this, with } = Str.replaceAll (Str.Pattern this) (Str.Replacement with)

escape :: Array String -> String -> String
escape specials =
  (specials <> ["\""])
  # map (\s -> replace { this: s, with: "\\" <> s })
  # foldr compose identity

encloseWith :: String -> String -> String -> String
encloseWith before after str = before <> str <> after

-- end util --
type ToPg a = a -> PgExpr

toPg_Int :: ToPg Int
toPg_Int = show >>> PgExpr

toPg_Number :: ToPg Number
toPg_Number = show >>> PgExpr

toPg_String :: ToPg String
toPg_String = PgExpr

toPg_Maybe :: ∀ a. ToPg a -> ToPg (Maybe a)
toPg_Maybe toPg = case _ of
  Nothing -> PgExpr "null"
  Just v -> toPg v
toPg_Array :: ∀ a. ToPg a -> ToPg (Array a)
toPg_Array toPg = map (toPg >>> un PgExpr) >>> map (escape ["{", ",", "}"]) >>> intercalate "," >>> encloseWith "{" "}" >>> PgExpr

toPg_Set :: ∀ a. Ord a => ToPg a -> ToPg (Set a)
toPg_Set toPg = Set.toUnfoldable >>> Array.sort >>> (toPg_Array toPg)

toPg_Tup0 :: ToPg (Tup Unit)
toPg_Tup0 _ = PgExpr "()"

toPg_Tup1 :: ∀ a. ToPg a -> ToPg (Tup a)
toPg_Tup1 toPg (Tup a) = toPg a # un PgExpr # (escape ["(", ",", ")"]) # encloseWith "(" ")" # PgExpr

toPg_Tup2 :: ∀ a b. ToPg a -> ToPg b -> ToPg (Tup (a /\ b))
toPg_Tup2 toPgA toPgB (Tup (a /\ b)) = [toPgA a, toPgB b] # map (un PgExpr) # intercalate "," # encloseWith "(" ")" # PgExpr

toPg_Tup3 :: ∀ a b c. ToPg a -> ToPg b -> ToPg c -> ToPg (Tup (a /\ b /\ c))
toPg_Tup3 toPgA toPgB toPgC (Tup (a /\ b /\ c)) = [toPgA a, toPgB b, toPgC c] # map (un PgExpr) # intercalate "," # encloseWith "(" ")" # PgExpr

toPg_Tup4 :: ∀ a b c d. ToPg a -> ToPg b -> ToPg c -> ToPg d -> ToPg (Tup (a /\ b /\ c /\ d))
toPg_Tup4 toPgA toPgB toPgC toPgD (Tup (a /\ b /\ c /\ d)) = [toPgA a, toPgB b, toPgC c, toPgD d] # map (un PgExpr) # intercalate "," # encloseWith "(" ")" # PgExpr
