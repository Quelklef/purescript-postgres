module Database.Postgres.Query
  ( query
  , atomically
  , PgErr(..)
  , queryThrow
  , query_
  , queryThrow_
  , exec
  , execThrow
  , exec_
  , execThrow_
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, throw) as Ex
import Control.Promise (Promise, toAffE)
import Control.Monad.Error.Class (class MonadError, throwError, catchError)
import Data.Bifunctor (lmap, rmap)
import Data.Newtype (un)
import Data.Traversable (traverse)
import Data.Either (Either(..))

import Database.Postgres.Internal.ParseComposite (parseComposite)
import Database.Postgres.Connection (Connection)
import Database.Postgres.ToPg (ToPg, toPg_Tup0)
import Database.Postgres.FromPg (class FromPg, fromPg, ParseErr)
import Database.Postgres.Types (PgExpr(..), Tup0, tup0)

foreign import query_f ::
  { conn :: Connection
  , sql :: String
  , params :: Array PgExpr
  } -> Effect (Promise (Array PgExpr))  -- (pg expr)[]

-- | Represents an error when executing a query
-- |
-- | The variants' semantics are as follows:
-- | - `PgErr_ParamErr`: invalid query params
-- | - `PgErr_ExecErr`: error came back from PostgreSQL
-- | - `PgErr_ResultErr`: failed to parse result rows
data PgErr
  = PgErr_ParamErr String
  | PgErr_ExecErr Ex.Error
  | PgErr_ResultErr ParseErr

toThrow :: forall m a. MonadAff m => Aff (Either PgErr a) -> m a
toThrow aff = liftAff $ aff >>= \a -> liftEffect $ case a of
  Left (PgErr_ParamErr e) -> Ex.throw $ show e
  Left (PgErr_ExecErr e) -> throwError e
  Left (PgErr_ResultErr e) -> Ex.throw $ show e
  Right val -> pure val

-- | Perform a query
-- |
-- | ```purescript
-- | do
-- |   let params = Tup $ 5 /\ 3
-- |   (r :: Number) <- conn # query "SELECT $1 * $2;" params
-- |   -- r == 15.0
-- | ```
-- |
-- | This s essentially a 3 step process:
-- | 1. Turn the params `p` into SQL expressions.
-- |    This can fail if you are not passing in a `Tup`.
-- |    Failures will be `PgErr_ParamErr`.
-- | 2. Execute the SQL. This can fail for any number of reasons.
-- |    Failures will be `PgErr_ExecErr`.
-- | 3. Parse the returned SQL expressions. This can fail if the result type `r`
-- |    doesn't match with the result rows. For instance, you are trying to parse
-- |    the result of `SELECT 1;` into a `String`.
-- |    Failures will be `PgErr_ResultErr`.
-- |
-- | Except in highly unusualy circumstances, the result should never fail
-- | in `m`. Instead, it should return a `Left`.
query ::
  forall p m r. MonadAff m => FromPg r =>
  ToPg p -> String -> p -> Connection -> m (Either PgErr (Array r))
query toPg sql params conn = liftAff $
  case parseParams (toPg params) of
    Left e -> pure (Left $ PgErr_ParamErr e)
    Right paramExprs -> do
      eitherResultExprs <- catchIntoEither $ toAffE $ query_f { conn, sql, params: paramExprs }
      pure $ do
        resultExprs <- eitherResultExprs # lmap PgErr_ExecErr
        results <- traverse fromPg resultExprs # lmap PgErr_ResultErr
        pure results

  where

  catchIntoEither :: forall m' e a. MonadError e m' => m' a -> m' (Either e a)
  catchIntoEither m = catchError (Right <$> m) (pure <<< Left)

  parseParams :: PgExpr -> Either String (Array PgExpr)
  parseParams expr =
    expr
    # parseComposite { open: "(", delim: ",", close: ")" }
    # lmap (\err ->
      "Failed to parse PostgreSQL parameters.\n"
      <> "This likely means you provided parameters as a value (:: a) instead of a row (:: Tup a).\n"
      <> "Parameter expression: " <> un PgExpr expr <> "\n"
      <> "Underlying parse error: " <> show err)

-- | Like `query`, but errors are thrown in `Aff`
queryThrow ::
  forall p m r. MonadAff m => FromPg r =>
  ToPg p -> String -> p -> Connection -> m (Array r)
queryThrow toPg sql params conn = toThrow $ query toPg sql params conn

-- | Like `query`, but no query parameters
query_ ::
  forall m r. MonadAff m => FromPg r =>
  String -> Connection -> m (Either PgErr (Array r))
query_ sql conn = query toPg_Tup0 sql tup0 conn

-- | Like `query_`, but errors are thrown in `Aff`
queryThrow_ ::
  forall m r. MonadAff m => FromPg r =>
  String -> Connection -> m (Array r)
queryThrow_ sql conn = toThrow $ query_ sql conn

-- | Like `query`, but no return value
exec ::
  forall p m. MonadAff m =>
  ToPg p -> String -> p -> Connection -> m (Either PgErr Unit)
exec toPg sql params conn = rmap (\(_ :: Array Tup0) -> unit) <$> query toPg sql params conn

-- | Like `exec`, but errors are thrown in `Aff`
execThrow ::
  forall p m. MonadAff m =>
  ToPg p -> String -> p -> Connection -> m Unit
execThrow toPg sql params conn = toThrow $ exec toPg sql params conn

-- | Like `exec`, but no query parameters or return value
exec_ ::
  forall m. MonadAff m =>
  String -> Connection -> m (Either PgErr Unit)
exec_ sql conn = exec toPg_Tup0 sql tup0 conn

-- | Like `exec_`, but errors are thrown in `Aff`
execThrow_ ::
  forall m. MonadAff m =>
  String -> Connection -> m Unit
execThrow_ sql conn = toThrow $ exec_ sql conn

-- | `conn # atomically f` is like `conn # f` except that `f` is preceeded
-- | by a `BEGIN` statement and terminated by a `COMMIT` statement if no
-- | errors occur or a `ROLLBACK` statement if they do.
atomically :: forall a. Aff a -> Connection -> Aff Unit
atomically act conn = do
  catchError
    (do
      conn # execThrow_ "BEGIN"
      void act
      conn # execThrow_ "COMMIT")
    (\err -> do
      conn # execThrow_ "ROLLBACK"
      throwError err)
