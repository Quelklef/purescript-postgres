module Database.Postgres.PgCodec
  ( PgCodec
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
  ) where

import Prelude

import Data.Maybe (Maybe (..), maybe, fromMaybe)
import Data.List (List)
import Data.List as List
import Data.Array (uncons) as Array
import Data.Foldable (intercalate)
import Data.Traversable (traverse)
import Data.Int (fromString) as Int
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
import Data.String.Common (replaceAll) as Str
import Data.String.Pattern (Pattern (..), Replacement (..)) as Str
import Data.Newtype (un)

import Database.Postgres.Types (Tup (..), PgExpr (..), Tup0)
import Database.Postgres.Types as Types
import Database.Postgres.Internal.ParseComposite (parseComposite) as PC


newtype PgCodec a = PgCodec
  { typename :: String
  , toPg :: a -> PgExpr
  , fromPg :: PgExpr -> Either ParseErr a
  }


replace :: { this :: String, with :: String } -> String -> String
replace { this, with } = Str.replaceAll (Str.Pattern this) (Str.Replacement with)

escape :: Array String -> String -> String
escape specials =
  (specials <> ["\""])
  # map (\s -> replace { this: s, with: "\\" <> s })
  # foldr compose identity

encloseWith :: String -> String -> String -> String
encloseWith before after str = before <> str <> after

parseComposite :: { open :: String, delim :: String, close :: String } -> PgExpr -> Either ParseErr (Array PgExpr)
parseComposite opts expr = PC.parseComposite opts expr # lmap (\issue -> mkErr (Just expr) issue)



-- | Integral-formatted number (eg, INT, SMALLINT, BIGINT)
int :: PgCodec Int
int = PgCodec
  { typename: "integral number (INT, SMALLINT, BIGINT)"
  , toPg: show >>> PgExpr
  , fromPg:
      contextualize "while parsing Int (SMALLINT, INT, BIGINT)"
      $ \expr -> Int.fromString (un PgExpr expr) # maybe (Left $ mkErr (Just expr) "Bad format") pure
  }


-- | Decimal-formatted number (eg, INT, FLOATING, DOUBLE PRECISION)
number :: PgCodec Number
number = PgCodec
  { typename: "decimal number"
  , toPg: show >>> PgExpr
  , fromPg:
      contextualize "while parsing Number (REAL, FIXED)"
      $ \expr -> Number.fromString (un PgExpr expr) # maybe (Left $ mkErr (Just expr) "Bad format") pure
  }

-- | Text (e.g., TEXT)
text :: PgCodec String
text = PgCodec
  { typename: "string"
  , toPg: PgExpr
  , fromPg: un PgExpr >>> pure
  }


-- | Boolean value
boolean :: PgCodec Boolean
boolean = PgCodec
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
nullable :: forall a. PgCodec a -> PgCodec (Maybe a)
nullable (PgCodec inner) = PgCodec
  { typename: "nullble " <> inner.typename
  , toPg: case _ of
      Nothing -> PgExpr "null"
      Just a -> inner.toPg a
  , fromPg: case _ of
      PgExpr "" -> pure Nothing
      expr -> Just <$> inner.fromPg expr
  }

-- | Array of things (type[])
arrayOf :: ∀ a. PgCodec a -> PgCodec (Array a)
arrayOf (PgCodec inner) = PgCodec
  { typename: "array of " <> inner.typename
  , toPg: map (inner.toPg >>> un PgExpr >>> escape ["{", ",", "}"]) >>> intercalate "," >>> encloseWith "{" "}" >>> PgExpr
  , fromPg:
      contextualize "while parsing Array"
      $ \expr -> do
        subExprs <- parseComposite { open: "{", delim: ",", close: "}" } expr
        vals <- traverse inner.fromPg subExprs
        pure vals
  }

-- | Set of things
-- |
-- | This is like an array but not
-- TODO: documentation
setOf :: forall a. Ord a => PgCodec a -> PgCodec (Set a)
setOf (PgCodec inner) =
  let PgCodec arrayCodec = arrayOf (PgCodec inner)
  in PgCodec
  { typename: "set of " <> inner.typename
  , toPg: Set.toUnfoldable >>> Array.sort >>> arrayCodec.toPg
  , fromPg:
      contextualize "while parsing Set"
      $ \expr -> do
        subExprs <- parseComposite { open: "{", delim: ",", close: "}" } expr
        vals <- traverse inner.fromPg subExprs
        pure $ Set.fromFoldable vals
  }

-- | Zero-tuple (unit)
tup0 :: PgCodec Tup0
tup0 =
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
tup1 :: forall a. PgCodec a -> PgCodec (Tup a)
tup1 (PgCodec inner) = PgCodec
  { typename: "Tup1 of " <> inner.typename
  , toPg: un Tup >>> inner.toPg >>> un PgExpr >>> (escape ["(", ",", ")"]) >>> encloseWith "(" ")" >>> PgExpr
  , fromPg:
      contextualize "while parsing Tup1"
      $ \expr -> do
        subExprs <- parseComposite { open: "(", delim: ",", close: ")" } expr
        subExpr <-
          case subExprs of
            [subExpr] -> Right subExpr
            _ -> Left $ mkErr (Just expr) "`fromPg_Tup1` incorrect number of `subExprs`"
        val <- inner.fromPg subExpr
        pure (Tup val)
  }

-- | Two-tuple
tup2 :: forall a b. PgCodec a -> PgCodec b -> PgCodec (Tup (a /\ b))
tup2 (PgCodec innerA) (PgCodec innerB) = PgCodec
  { typename: "Tup2 of (" <> innerA.typename <> " /\ " <> innerB.typename <> ")"
  , toPg: \(Tup (a /\ b)) -> [innerA.toPg a, innerB.toPg b] # map (un PgExpr) # intercalate "," # encloseWith "(" ")" # PgExpr
  , fromPg:
      contextualize "while parsing Tup2"
      $ \expr -> do
        subExprs <- parseComposite { open: "(", delim: ",", close: ")" } expr
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
  PgCodec c1 -> PgCodec c2 -> PgCodec c3 -> PgCodec c4
  -> PgCodec (Tup (c1 /\ c2 /\ c3 /\ c4))
tup4 (PgCodec c1) (PgCodec c2) (PgCodec c3) (PgCodec c4) = PgCodec
  { typename:
      "Tup4 of (" <> intercalate " /\ " [c1.typename, c2.typename, c3.typename, c4.typename] <> ")"
  , toPg: \(Tup (v1 /\ v2 /\ v3 /\ v4)) -> [c1.toPg v1, c2.toPg v2, c3.toPg v3, c4.toPg v4] # map (un PgExpr) # intercalate "," # encloseWith "(" ")" # PgExpr
  , fromPg:
      contextualize "while parsing Tup4"
      $ \expr -> do
        subExprs <- parseComposite { open: "(", delim: ",", close: ")" } expr
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
fromPg :: forall a. PgCodec a -> PgExpr -> Either ParseErr a
fromPg (PgCodec inner) = inner.fromPg >>> lmap finalizeErr
  where finalizeErr (ParseErr err) = ParseErr $ err { typename = Just inner.typename }

toPg :: forall a. PgCodec a -> a -> PgExpr
toPg (PgCodec codec) = codec.toPg

