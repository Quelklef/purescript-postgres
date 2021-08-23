module Database.Postgres.Types
  ( PgExpr(..)
  , Tup(..)
  , Tup0
  , tup0
  ) where

import Prelude

import Data.Newtype (class Newtype)


-- | Used as documentation to communicate that a string is intended to
-- | contain a PostgreSQL expression.
-- |
-- | This is not necessarily enforced by e.g. a smart constructor.
newtype PgExpr = PgExpr String

derive instance newtype_PgExpr :: Newtype PgExpr _


-- | Represents an SQL composite type (aka row)
-- |
-- | Use as follows:
-- | - For a size-0 row, use `Tup Unit`, aka `Tup0`
-- | - For a size-1 row of a, use `Tup a`
-- | - For a size-2+ row of `a`, `b`, ..., use `Tup (a /\ b /\ ...)`
-- |
-- | Called `Tup` instead of `Row` to avoid naming conflicts with `Prim.Row`
newtype Tup a = Tup a

-- | Alias for `Tup Unit`
type Tup0 = Tup Unit

-- | The single `Tup0` value
tup0 :: Tup0
tup0 = Tup unit
