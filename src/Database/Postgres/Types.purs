module Database.Postgres.Types
  ( PgExpr(..)
  , QueryValue
  , Tup(..)
  , Tup0
  , tup0
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)


-- | Used as documentation to communicate that a string is intended to
-- | contain a PostgreSQL expression.
-- |
-- | This is not necessarily enforced by e.g. a smart constructor.
newtype PgExpr = PgExpr String

derive instance newtype_PgExpr :: Newtype PgExpr _
derive instance generic_PgExpr :: Generic PgExpr _
instance show_PgExpr :: Show PgExpr where show = genericShow
derive instance eq_PgExpr :: Eq PgExpr
derive instance ord_PgExpr :: Ord PgExpr


-- | Type for values that can be used as SQL parameters, and
-- | values that can be returned from SQL executions.
-- |
-- | The Nothing value represents SQL `null`. All other SQL
-- | values are represented as textual expressions
type QueryValue = Maybe PgExpr


-- | Represents an SQL composite type (aka row)
-- |
-- | Use as follows:
-- | - For a size-0 row, use `Tup Unit`, aka `Tup0`
-- | - For a size-1 row of a, use `Tup a`
-- | - For a size-2+ row of `a`, `b`, ..., use `Tup (a /\ b /\ ...)`
-- |
-- | Called `Tup` instead of `Row` to avoid naming conflicts with `Prim.Row`
newtype Tup a = Tup a

derive instance newtype_Tup :: Newtype (Tup a) _
derive instance generic_Tup :: Generic (Tup a) _
instance show_Tup :: Show a => Show (Tup a) where show = genericShow
derive instance eq_Tup :: Eq a => Eq (Tup a)
derive instance ord_Tup :: Ord a => Ord (Tup a)

-- | Alias for `Tup Unit`
type Tup0 = Tup Unit

-- | The single `Tup0` value
tup0 :: Tup0
tup0 = Tup unit
