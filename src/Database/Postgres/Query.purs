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
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Bifunctor (lmap, rmap)
import Data.Newtype (un)
import Data.Traversable (traverse)
import Data.Either (Either (..))

import Database.Postgres.Connection (Connection)
import Database.Postgres.PgCodec (RowCodec, PgCodec, fromPg, toPg, ParseErr)
import Database.Postgres.PgCodec as PgCodec
import Database.Postgres.Types (PgExpr (..), Tup0, tup0)


foreign import query_f ::
  { conn :: Connection
  , sql :: String
  , params :: Array (Nullable PgExpr)
  } -> Effect (Promise (Array (Array (Nullable PgExpr))))

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
  Left (PgErr_ParamErr e) -> Ex.throw $ ("`PgErr_ParamErr`: " <> show e)
  Left (PgErr_ExecErr e) -> throwError e
  Left (PgErr_ResultErr e) -> Ex.throw $ ("`PgErr_ResultErr`: " <> show e)
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
  forall params m r. MonadAff m =>
  RowCodec params -> RowCodec r -> String -> params -> Connection -> m (Either PgErr (Array r))
query inCodec outCodec sql params conn = liftAff $
  do
    eitherResultExprs <- catchIntoEither $ toAffE $ query_f { conn, sql, params: paramExprs }
    pure $ do
      resultExprs <- eitherResultExprs # lmap PgErr_ExecErr
      let resultExprs' = resultExprs # (map <<< map) Nullable.toMaybe
      results <- traverse (fromPg outCodec) resultExprs' # lmap PgErr_ResultErr
      pure results

  where

  catchIntoEither :: forall m' e a. MonadError e m' => m' a -> m' (Either e a)
  catchIntoEither m = catchError (Right <$> m) (pure <<< Left)

  paramExprs :: Array (Nullable PgExpr)
  paramExprs = (toPg inCodec) params # map Nullable.toNullable


-- | Like `query`, but errors are thrown in `Aff`
queryThrow ::
  forall p m r. MonadAff m =>
  RowCodec p -> RowCodec r -> String -> p -> Connection -> m (Array r)
queryThrow inCodec outCodec sql params conn = toThrow $ query inCodec outCodec sql params conn

-- | Like `query`, but no query parameters
query_ ::
  forall m r. MonadAff m =>
  RowCodec r -> String -> Connection -> m (Either PgErr (Array r))
query_ outCodec sql conn = query PgCodec.row0 outCodec sql unit conn

-- | Like `query_`, but errors are thrown in `Aff`
queryThrow_ ::
  forall m r. MonadAff m =>
  RowCodec r -> String -> Connection -> m (Array r)
queryThrow_ codec sql conn = toThrow $ query_ codec sql conn

-- | Like `query`, but no return value
exec ::
  forall p m. MonadAff m =>
  RowCodec p -> String -> p -> Connection -> m (Either PgErr Unit)
exec inCodec sql params conn = rmap (\(_ :: Array Unit) -> unit) <$> query inCodec PgCodec.row0 sql params conn

-- | Like `exec`, but errors are thrown in `Aff`
execThrow ::
  forall p m. MonadAff m =>
  RowCodec p -> String -> p -> Connection -> m Unit
execThrow codec sql params conn = toThrow $ exec codec sql params conn

-- | Like `exec`, but no query parameters or return value
exec_ ::
  forall m. MonadAff m =>
  String -> Connection -> m (Either PgErr Unit)
exec_ sql conn = exec PgCodec.row0 sql unit conn

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
