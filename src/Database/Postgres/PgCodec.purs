module Database.Postgres.PgCodec
  ( PgCodec
  , RowCodec
  , PgRow
  , fromPg
  , toPg
  {- , mkImpl -}
  , ParseErr (..)

  , int
  , number
  , boolean
  , text
  , nullable
  , arrayOf
  , setOf
  , tup0
  , tup1
  , tup2
  , tup4
  , row0
  , row1
  , row2
  ) where

import Prelude

import Data.Maybe (Maybe (..), maybe, fromMaybe)
import Data.List (List)
import Data.List as List
import Data.Array (uncons) as Array
import Data.Foldable (intercalate)
import Data.Traversable (traverse)
import Data.Int (fromString) as Int
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Number (fromString) as Number
import Data.Newtype (un)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Either (Either (..))
import Data.Bifunctor (lmap)
import Data.Set (Set)
import Data.Set (fromFoldable) as Set
import Data.Foldable (intercalate, foldr)
import Data.Maybe (Maybe (..))
import Data.Set (Set)
import Data.Set as Set
import Data.Array as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.String.Common (replaceAll, toLower) as Str
import Data.String.Pattern (Pattern (..), Replacement (..)) as Str
import Data.Newtype (un)

import Database.Postgres.Types (Tup (..), PgExpr (..), Tup0, QueryValue)
import Database.Postgres.Types as Types
import Database.Postgres.Internal.ParseComposite (parseArray, parseTuple) as PC


newtype PgCodec pg a = PgCodec
  { typename :: String
  , toPg :: a -> pg
  , fromPg :: pg -> Either ParseErr a
  }


type PgRow = Array QueryValue
type RowCodec = PgCodec PgRow
type FieldCodec = PgCodec QueryValue

replace :: { this :: String, with :: String } -> String -> String
replace { this, with } = Str.replaceAll (Str.Pattern this) (Str.Replacement with)

escape :: Array String -> String -> String
escape specials =
  (specials <> ["\""])
  # map (\s -> replace { this: s, with: "\\" <> s })
  # foldr compose identity

encloseWith :: String -> String -> String -> String
encloseWith before after str = before <> str <> after

parseArray :: PgExpr -> Either ParseErr (Array QueryValue)
parseArray expr = PC.parseArray expr # lmap (\issue -> mkErr (Just expr) issue)
parseTuple :: PgExpr -> Either ParseErr (Array QueryValue)
parseTuple expr = PC.parseTuple expr # lmap (\issue -> mkErr (Just expr) issue)


dealWithTheNullsPlease :: forall a. PgCodec PgExpr a -> PgCodec QueryValue a
dealWithTheNullsPlease (PgCodec c) = PgCodec
  { typename: c.typename
  , toPg: c.toPg >>> Just
  , fromPg: \mExpr -> case mExpr of
        Nothing -> Left $ mkErr mExpr ("Got null but expected value of type " <> c.typename)
        Just expr -> c.fromPg expr
  }


-- | Integral-formatted number (eg, INT, SMALLINT, BIGINT)
int :: FieldCodec Int
int = dealWithTheNullsPlease $ PgCodec
  { typename: "integral number (INT, SMALLINT, BIGINT)"
  , toPg: show >>> PgExpr
  , fromPg:
      contextualize "while parsing Int (SMALLINT, INT, BIGINT)"
      $ \expr -> Int.fromString (un PgExpr expr) # maybe (Left $ mkErr (Just expr) "Bad format") pure
  }


-- | Decimal-formatted number (eg, INT, FLOATING, DOUBLE PRECISION)
number :: FieldCodec Number
number = dealWithTheNullsPlease $ PgCodec
  { typename: "decimal number"
  , toPg: show >>> PgExpr
  , fromPg:
      contextualize "while parsing Number (REAL, FIXED)"
      $ \expr -> Number.fromString (un PgExpr expr) # maybe (Left $ mkErr (Just expr) "Bad format") pure
  }

-- | Text (e.g., TEXT)
text :: FieldCodec String
text = dealWithTheNullsPlease $ PgCodec
  { typename: "string"
  , toPg: PgExpr
  , fromPg: un PgExpr >>> pure
  }


-- | Boolean value
boolean :: FieldCodec Boolean
boolean = dealWithTheNullsPlease $ PgCodec
  { typename: "boolean"
  , toPg: case _ of
      true -> PgExpr "t"
      false -> PgExpr "f"
  , fromPg:
      contextualize "while parsing Boolean (BOOL)"
      $ \expr -> case un PgExpr expr of
        "t" -> pure true
        "f" -> pure false
        _ -> Left $ mkErr (Just expr) "Expected 't' or 'f'"
  }


-- | Nullable value
nullable :: forall a. FieldCodec a -> FieldCodec (Maybe a)
nullable (PgCodec inner) = PgCodec
  { typename: "nullable " <> inner.typename
  , toPg: case _ of
      Nothing -> Nothing
      Just a -> inner.toPg a
  , fromPg: case _ of
      Nothing -> pure Nothing
      Just expr -> Just <$> inner.fromPg (Just expr)
  }

-- | Array of things (type[])
arrayOf :: ∀ a. FieldCodec a -> FieldCodec (Array a)
arrayOf (PgCodec inner) = PgCodec
  { typename: "array of " <> inner.typename
  , toPg: map (inner.toPg >>> toElement) >>> intercalate "," >>> encloseWith "{" "}" >>> PgExpr >>> Just
  , fromPg:
      contextualize "while parsing Array"
      $ case _ of
          Nothing -> Left $ mkErr Nothing "`arrayOf` got a null"
          Just expr -> do
            subExprs <- parseArray expr
            vals <- traverse inner.fromPg subExprs
            pure vals
  }

  where

  -- Prints/escapes QueryValue for use in PostgreSQL ARRAY.
  --
  -- In more words, for any QueryValue `v`, the value `toElement v` is
  -- a PostgreSQL expression string which:
  --   (1) is appropriate for use as an ARRAY element
  --   (2) denotes `s`
  toElement Nothing = "null"
  toElement (Just (PgExpr str))
    | Str.toLower str == "null" = str # encloseWith "\"" "\""
    | str == "" = "\"\""
    | otherwise = str # escape ["{", ",", "}", "\\", "\""]


-- | Set of things
-- |
-- | In more words, this is like an array but not
setOf :: forall a. Ord a => FieldCodec a -> FieldCodec (Set a)
setOf (PgCodec inner) =
  let PgCodec arrayCodec = arrayOf (PgCodec inner)
  in PgCodec
  { typename: "set of " <> inner.typename
  , toPg: Set.toUnfoldable >>> Array.sort >>> arrayCodec.toPg
  , fromPg:
      contextualize "while parsing Set"
      $ case _ of
          Nothing -> Left $ mkErr Nothing "`setOf` got null"
          Just expr -> do
            subExprs <- parseArray expr
            vals <- traverse inner.fromPg subExprs
            pure $ Set.fromFoldable vals
  }

-- | Zero-tuple (unit)
tup0 :: FieldCodec Tup0
tup0 = dealWithTheNullsPlease $
  PgCodec
    { typename: "Tup0"
    , toPg: \_ -> PgExpr "()"
    , fromPg:
        \expr@(PgExpr str) ->
          case str of
              "()" -> Right Types.tup0
              _ -> Left $ mkErr (Just expr) "`fromPg_Tup0` expected `\"()\"`"
    }


-- | One-tuple
tup1 :: forall a. FieldCodec a -> FieldCodec (Tup a)
tup1 (PgCodec inner) = PgCodec
  { typename: "Tup1 of " <> inner.typename
  , toPg: un Tup >>> inner.toPg >>> maybe "" (un PgExpr) >>> escape ["(", ",", ")"] >>> encloseWith "(" ")" >>> PgExpr >>> Just
  , fromPg:
      contextualize "while parsing a Tup1"
      $ case _ of
          Nothing -> Left $ mkErr Nothing "`tup1` got null ☹"
          Just expr -> do
            subExprs <- parseTuple expr
            subExpr <-
              case subExprs of
                [subExpr] -> Right subExpr
                _ -> Left $ mkErr (Just expr) "`tup1` incorrect number of `subExprs`"
            val <- inner.fromPg subExpr
            pure (Tup val)
  }

-- | Two-tuple
tup2 :: forall a b. FieldCodec a -> FieldCodec b -> FieldCodec (Tup (a /\ b))
tup2 (PgCodec innerA) (PgCodec innerB) = PgCodec
  { typename: "Tup2 of (" <> innerA.typename <> """ /\ """ <> innerB.typename <> ")"
  , toPg: \(Tup (a /\ b)) -> [innerA.toPg a, innerB.toPg b] # map (maybe "" (un PgExpr)) # intercalate "," # encloseWith "(" ")" # PgExpr # Just
  , fromPg:
      contextualize "while parsing Tup2"
      $ case _ of
          Nothing -> Left $ mkErr Nothing "`tup2` got null"
          Just expr -> do
            subExprs <- parseTuple expr
            (subExpr1 /\ subExpr2) <-
              case subExprs of
                [subExpr1, subExpr2] -> Right (subExpr1 /\ subExpr2)
                _ -> Left $ mkErr (Just expr) "`fromPg_Tup2` incorrect number of `subExprs`"
            val1 <- innerA.fromPg subExpr1
            val2 <- innerB.fromPg subExpr2
            pure (Tup (val1 /\ val2))
  }

-- | Four-tuple (!)
tup4 :: forall c1 c2 c3 c4.
  FieldCodec c1 -> FieldCodec c2 -> FieldCodec c3 -> FieldCodec c4
  -> FieldCodec (Tup (c1 /\ c2 /\ c3 /\ c4))
tup4 (PgCodec c1) (PgCodec c2) (PgCodec c3) (PgCodec c4) = PgCodec
  { typename:
      "Tup4 of (" <> intercalate """ /\ """ [c1.typename, c2.typename, c3.typename, c4.typename] <> ")"
  , toPg: \(Tup (v1 /\ v2 /\ v3 /\ v4)) -> [c1.toPg v1, c2.toPg v2, c3.toPg v3, c4.toPg v4] # map (maybe "" (un PgExpr)) # intercalate "," # encloseWith "(" ")" # PgExpr # Just
  , fromPg:
      contextualize "while parsing Tup4"
      $ case _ of
          Nothing -> Left $ mkErr Nothing "`tup4` got null"
          Just expr -> do
            subExprs <- parseTuple expr
            (e1 /\ e2 /\ e3 /\ e4) <-
              case subExprs of
                [e1, e2, e3, e4] -> Right (e1 /\ e2 /\ e3 /\ e4)
                _ -> Left $ mkErr (Just expr) "`tup4` incorrect number of `subExprs`"
            v1 <- c1.fromPg e1
            v2 <- c2.fromPg e2
            v3 <- c3.fromPg e3
            v4 <- c4.fromPg e4
            pure (Tup (v1 /\ v2 /\ v3 /\ v4))
  }


row0 :: RowCodec Unit
row0 = PgCodec
  { typename: "size-0 row"
  , toPg: \_ -> []
  , fromPg: case _ of
      [] -> pure unit
      row -> Left $ mkErr Nothing ("expected size-0 row, got row of size " <> show (Array.length row))
  }

row1 :: forall t.
  FieldCodec t -> RowCodec t
row1 (PgCodec f1) = PgCodec
  { typename: "size-1 row of " <> f1.typename
  , toPg: \x -> [ f1.toPg x ]
  , fromPg: case _ of
      [x1] -> f1.fromPg x1
      row -> Left $ mkErr Nothing ("expected size-1 row,got row of size " <> show (Array.length row))
  }

row2 :: forall t1 t2.
  FieldCodec t1 -> FieldCodec t2 -> RowCodec (t1 /\ t2)
row2 (PgCodec f1) (PgCodec f2) = PgCodec
  { typename: "size-2 row of (" <> f1.typename <> ", " <> f2.typename <> ")"
  , toPg: \(x1 /\ x2) -> [ f1.toPg x1, f2.toPg x2 ]
  , fromPg: case _ of
      [x1, x2] -> (/\) <$> f1.fromPg x1 <*> f2.fromPg x2
      row -> Left $ mkErr Nothing ("expected size-2 row, got row of size " <> show (Array.length row))
  }




{-
-- | Class of types which can be parsed out of SQL expressions
-- |
-- | The type is pretty arcane, so let's start with example usage:
-- |
-- TODO fix
-- | ```purescript
-- | newtype Box = Box { width :: Number, height :: Number }
-- |
-- | instance FromPg Box where
-- |   impl = mkImpl
-- |     $ \(width /\ height) ->
-- |       if width < 0.0 || height < 0.0
-- |       then Left "Cannot have a box with negative dimensions"
-- |       else Right $ Box { width, height }
-- | ```
-- |
-- | For an instance `FromPg a`, the single class method
-- | is `impl :: Impl a`, which must always be created using `mkImpl.`
-- | This strange setup is for secret technical reasons.
-- |
-- | `mkImpl` requests a parser of type `FromPg p => (p -> Either String a)`.
-- | In other words, a function from some type `p` that we already know
-- | how to parse to `Either` a `String` error or a result `a`.
-- |
-- | Make use of `FromPg` instances using `fromPg`.
class FromPg a where
  impl :: Impl a
-- Keep the type opaque because:
--  a) Since the set of SQL expressions is a known domain, the
--     responsibility of parsing them should fall on this
--     library code. The client should not have to--or even
--     be allowed to--think about SQL expressions.
--  b) Restricting access to the SQL expressions gives better
--     future compatibility for becoming database-polymorphic.
-}

-- Morally a String -> Either err res
--
-- Note that
--   String -> Either err res                           [1]
-- is a strange type for a parser; more common is something like
--   String -> Either err { res: res, rest: String }    [2]
-- The reason for using type [1] instead of [2] is as follows.
--
-- Most modern grammars exhibit behaviour wherein in order to parse
-- the structure of composite types, the elements must be parsed as
-- well. For instance, the Purescript array
--   [ "one, two", "three" ]
-- Cannot be parsed without parsing the contained strings, because
-- by parsing the strings we learn that the first comma is *not*
-- a part of the array syntax; there are two elements, not three.
-- Grammar like this demands parser types of form [2].
--
-- On the contrary, PostgreSQL grammar *does* allow for containers
-- to be parsed before their elements. We can parse the array string
--   {<(0\,0)\,1>,<(1\,1)\,2>}
-- into the element strings
--   <(0,0),1>  and  <(1,1),2>
-- before knowing what type they are inteded to be (CIRCLE? TEXT?).
-- Because of this grammatical quirk, we are able to get away with
-- using the simpler parser type [1].

-- | Represents a parse error
-- |
-- | Has the following attributes:
-- | - `issue`: represents the actual error.
-- | - `culprit`: the problematic SQL expression.
-- | - `context`: a list of human-readable messages contexualizing the error.
-- |              Earlier messages also contextualize later messages.
-- | - `typename`: a human-readable description of the SQL type we're trying to parse.
data ParseErr = ParseErr
  { issue :: String
  , context :: List String
  , culprit :: Maybe PgExpr
  , typename :: Maybe String
  -- ^ It's actually misleading to have this on ParseErr, since it's set
  --   not during parsing but rather at the top-level parse function.
  --   It's on this type for code convenience.
  }

-- end core --

instance show_ParseErr :: Show ParseErr where
  show = \(ParseErr err) -> intercalate "\n" $
    [ "Failed to parse PostgreSQL expression!" ]
    <>
    (case err.culprit of
      Nothing -> []
      Just (PgExpr culp) -> ["Expression: " <> culp])
    <>
    [ "Error: " <> err.issue
    , List.reverse err.context # map ("... " <> _) # intercalate "\n"
    , "... while parsing expression as " <> fromMaybe "<unknown>" err.typename
    ]

mkErr :: Maybe PgExpr -> String -> ParseErr
mkErr culprit issue = ParseErr { issue, culprit, context: mempty, typename: Nothing }

contextualize :: forall s a. String -> (s -> Either ParseErr a) -> (s -> Either ParseErr a)
contextualize ctx parse expr = parse expr # lmap mapErr
  where mapErr (ParseErr err) = ParseErr $ err { context = List.Cons ctx err.context }

{-
-- User-defined Codec
mkImpl :: forall a b. PgCodec a -> (a -> Either String b) -> PgCodec b
mkImpl (PgCodec { parser: parserA, typename: typenameA }) parserAToB =
  PgCodec
    { typename: typenameA  -- user types inherit pg typenames
    , fromPg:
        \expr -> do
          val <- parserA expr
          let parser' = contextualize "After a successful parse"
                      $ parserAToB <#> lmap \issue -> mkErr Nothing issue
          res <- parser' val
          pure res
    }
-}

-- | Parse a `PgExpr`
fromPg :: forall pg a. PgCodec pg a -> pg -> Either ParseErr a
fromPg (PgCodec inner) = inner.fromPg >>> lmap finalizeErr
  where finalizeErr (ParseErr err) = ParseErr $ err { typename = Just inner.typename }

toPg :: forall pg a. PgCodec pg a -> a -> pg
toPg (PgCodec codec) = codec.toPg
