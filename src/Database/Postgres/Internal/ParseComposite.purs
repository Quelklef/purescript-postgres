-- | Leaked implementation detail
module Database.Postgres.Internal.ParseComposite (parseComposite) where
  
import Prelude

import Data.Either (Either(..))

import Database.Postgres.Types (PgExpr)

-- Parse a PostgreSQL expression denoting a composite type (arrays, rows) into its elements
foreign import parseComposite_f
  :: { left :: forall a b. a -> Either a b, right :: forall a b. b -> Either a b }
  -> { open :: String, delim :: String, close :: String } -> PgExpr -> Either String (Array PgExpr)

parseComposite :: { open :: String, delim :: String, close :: String } -> PgExpr -> Either String (Array PgExpr)
parseComposite opts expr = parseComposite_f { left: Left, right: Right } opts expr
