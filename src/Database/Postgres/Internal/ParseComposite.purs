-- | Leaked implementation detail
module Database.Postgres.Internal.ParseComposite (parseArray, parseTuple) where

import Prelude

import Data.Either (Either (..))
import Data.Maybe (Maybe (..))

import Database.Postgres.Types (PgExpr, QueryValue)

-- Parse a PostgreSQL expression denoting a composite type (arrays, rows) into its elements
foreign import parseComposite_f
  :: { left :: forall a b. a -> Either a b
     , right :: forall a b. b -> Either a b
     , just :: forall a. a -> Maybe a
     , nothing :: forall a. Maybe a
     }
  -> { open :: String
     , delim :: String
     , close :: String
     , exprIsNull :: String -> Boolean
     , escapeStyle :: String
     }
  -> PgExpr
  -> Either String (Array QueryValue)

parseArray :: PgExpr -> Either String (Array QueryValue)
parseArray expr = parseComposite_f
  { left: Left, right: Right, just: Just, nothing: Nothing }
  { open: "{", delim: ",", close: "}", exprIsNull: (_ == "NULL"), escapeStyle: "backslash" }
  expr

parseTuple :: PgExpr -> Either String (Array QueryValue)
parseTuple expr = parseComposite_f
  { left: Left, right: Right, just: Just, nothing: Nothing }
  { open: "(", delim: ",", close: ")", exprIsNull: (_ == ""), escapeStyle: "double" }
  expr
