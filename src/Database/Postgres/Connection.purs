module Database.Postgres.Connection
  ( Connection
  , open
  ) where

import Prelude

import Effect (Effect)
import Control.Promise (Promise, toAffE)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)

-- | Represents a connection to a PostgreSQL database
foreign import data Connection :: Type
foreign import open_f :: String -> Effect (Promise Connection)

-- | Open a connection to a PostgreSQL database
-- |
-- | Expects a [connection string](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING) as input.
open :: String -> Aff Connection
open connectionString = liftAff <<< toAffE $ open_f connectionString
