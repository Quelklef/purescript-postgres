module Database.Postgres.FromPg
  ( FromPg
  , fromPg
  , mkImpl
  , ParseErr(..)
  , fromPg_Tup0
  , fromPg_Tup1
  , fromPg_Tup2
  , fromPg_Tup4
  , fromPg_Boolean
  , fromPg_Int
  , fromPg_Number
  , fromPg_Maybe
  , fromPg_String
  , fromPg_Array
  , fromPg_Set

  -- v Forced exports
  --, class InnerTup
  --, impl_inner
  ) where

import Prelude

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.List (List)
import Data.List as List
import Data.Array (uncons) as Array
import Data.Foldable (intercalate)
import Data.Traversable (traverse)
import Data.Int (fromString) as Int
import Data.Number (fromString) as Number
import Data.Newtype (un)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Either (Either(..))
import Data.Bifunctor (lmap)
import Data.Set (Set)
import Data.Set (fromFoldable) as Set

import Database.Postgres.Types (Tup(..), PgExpr(..), Tup0, tup0)
import Database.Postgres.Internal.ParseComposite (parseComposite) as PC

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

-- | Represents a `FromPg` implementation.
-- | This type is purposefully opaque.
-- | See the `FromPg` docs for usage.
newtype FromPg a = FromPg
  { parser :: ExprParser a
  , typename :: String
  }

type ExprParser a = PgExpr -> Either ParseErr a
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

mkImpl :: forall a b. FromPg a -> (a -> Either String b) -> FromPg b
mkImpl (FromPg { parser: parserA, typename: typenameA }) parserAToB =
  FromPg
    { typename: typenameA  -- user types inherit pg typenames
    , parser:
        \expr -> do
          val <- parserA expr
          let parser' = contextualize "After a successful parse"
                      $ parserAToB <#> lmap \issue -> mkErr Nothing issue
          res <- parser' val
          pure res
    }

-- | Parse a `PgExpr`
fromPg :: forall a. FromPg a -> PgExpr -> Either ParseErr a
fromPg (FromPg {parser, typename}) expr =
  parser expr # lmap finalizeErr
  where
  finalizeErr (ParseErr err) = ParseErr $ err { typename = Just typename }

parseComposite :: { open :: String, delim :: String, close :: String } -> PgExpr -> Either ParseErr (Array PgExpr)
parseComposite opts expr = PC.parseComposite opts expr # lmap (\issue -> mkErr (Just expr) issue)

{-
-- begin instances --


-- | Parsing into `PgExpr` will return the expression unchanged
instance fromPg_PgExpr :: FromPg PgExpr where
  impl = Impl { typename: "postgresql expression", parser: pure }

-- | Parse as text (e.g., TEXT)
instance fromPg_String :: FromPg String where
  impl = Impl { typename: "string", parser: un PgExpr >>> pure }

-- | Parse a boolean
instance fromPg_Boolean :: FromPg Boolean where
  impl = Impl
    { typename: "boolean"
    , parser:
        contextualize "while parsing Boolean (BOOL)"
        $ \expr -> case un PgExpr expr of
          "t" -> pure true
          "f" -> pure false
          _ -> Left $ mkErr (Just expr) "Expected 't' or 'f'"
    }

-- | Parse a decimal-format number (e.g., INT, FLOATING, DOUBLE PRECISION)
instance fromPg_Number :: FromPg Number where
  impl = Impl
    { typename: "decimal number"
    , parser:
        contextualize "while parsing Number (REAL, FIXED)"
        $ \expr -> Number.fromString (un PgExpr expr) # maybe (Left $ mkErr (Just expr) "Bad format") pure
    }

-- | Parse an integral-format number (e.g., INT, BIGINT)
instance fromPg_Int :: FromPg Int where
  impl = Impl
    { typename: "integral number"
    , parser:
        contextualize "while parsing Int (SMALLINT, INT, BIGINT)"
        $ \expr -> Int.fromString (un PgExpr expr) # maybe (Left $ mkErr (Just expr) "Bad format") pure
    }

-- | Parse an `a` or NULL
instance fromPg_Maybe :: FromPg a => FromPg (Maybe a) where
  impl =
    let Impl innerImpl = (impl :: Impl a)
    in Impl
      { typename: "nullable " <> innerImpl.typename
      , parser: case _ of
          PgExpr "" -> pure Nothing
          expr -> Just <$> innerImpl.parser expr
      }

-- | Parse an array
instance fromPg_Array :: FromPg a => FromPg (Array a) where
  impl =
    let Impl innerImpl = (impl :: Impl a)
    in Impl
      { typename: "array of " <> innerImpl.typename
      , parser:
          contextualize "while parsing Array"
          $ \expr -> do
            subExprs <- parseComposite { open: "{", delim: ",", close: "}" } expr
            vals <- traverse innerImpl.parser subExprs
            pure vals
      }

-- | Parse an array, then turn it into a set
instance fromPg_Set :: (Ord a, FromPg a) => FromPg (Set a) where
  impl =
    let Impl innerImpl = (impl :: Impl a)
    in Impl
      { typename: "set of " <> innerImpl.typename
      , parser:
          contextualize "while parsing Set"
          $ \expr -> do
            subExprs <- parseComposite { open: "{", delim: ",", close: "}" } expr
            vals <- traverse innerImpl.parser subExprs
            pure $ Set.fromFoldable vals
      }

-- | Parse a PostgreSQL composite type.
-- | For instance, to parse (INT, TEXT, BOOL), use the `FromPg` instance for `Tup (Int /\ String /\ Boolean)`
-- |
-- | Also see the documentation for `Database.Postgres.Types (Tup)`
instance fromPg_Tup :: InnerTup a => FromPg (Tup a) where
  impl =
    let innerImpl = (impl_inner :: InnerTup_Impl a)
    in Impl
      { typename: "tuple of (" <> innerImpl.typename <> ")"
      , parser:
          contextualize "while parsing composite type (aka row, record)"
          $ \expr -> parseComposite { open: "(", delim: ",", close: ")" } expr >>= innerImpl.parser { idx: 0 } # map Tup
      }

-- | Leaked implementation detail
class InnerTup a where
  impl_inner :: InnerTup_Impl a

type InnerTup_Impl a =
    { parser :: { idx :: Int } -> Array PgExpr -> Either ParseErr a
    , typename :: String
    }

instance fromPgInnerTup_recur :: (FromPg a, InnerTup b) => InnerTup (a /\ b) where
  impl_inner =
    let Impl aImpl = (impl :: Impl a)
        aParser = aImpl.parser
        bImpl = (impl_inner :: InnerTup_Impl b)
        bParser = bImpl.parser
    in
      { typename: aImpl.typename <> ", " <> bImpl.typename
      , parser:
        \{ idx } ->
          contextualize ("while parsing element #" <> show idx)
          $ \exprs -> case Array.uncons exprs of
            Nothing -> Left $ mkErr Nothing "Expected an element"
            Just { head, tail } -> do
              headVal <- aParser head
              tailVal <- bParser { idx: idx + 1 } tail
              pure $ headVal /\ tailVal
      }

else instance fromPgInnerTup_empty :: InnerTup Unit where
  impl_inner =
    { typename: "<empty>" -- TODO: what to do here??? Tup0 seems to be exceptional in a number of places
    , parser:
        \_ exprs -> case Array.uncons exprs of
          Nothing -> pure unit
          Just { head, tail: _ } -> Left $ mkErr (Just head) "Expected no elements"
    }

else instance fromPgInnerTup_base :: FromPg a => InnerTup a where
  impl_inner =
    let Impl aImpl = (impl :: Impl a)
        aParser = aImpl.parser
    in
      { typename: aImpl.typename
      , parser:
          \_ exprs -> case exprs of
            [expr] -> aParser expr
            _ -> Left $ mkErr Nothing "Unexpected extraneous elements (too many or zero values!)"
      }
-}

fromPg_Boolean :: FromPg Boolean
fromPg_Boolean =
  FromPg
    { typename: "boolean"
    , parser:
        contextualize "while parsing Boolean (BOOL)"
        $ \expr -> case un PgExpr expr of
          "t" -> pure true
          "f" -> pure false
          _ -> Left $ mkErr (Just expr) "Expected 't' or 'f'"
    }

fromPg_Tup0 :: FromPg Tup0
fromPg_Tup0 =
  FromPg
    { typename: "Tup0"
    , parser:
        \expr@(PgExpr str) ->
          case str of
              "()" -> Right tup0
              _ -> Left $ mkErr (Just expr) "`fromPg_Tup0` expected `\"()\"`"
    }

fromPg_Tup1 :: forall a. FromPg a -> FromPg (Tup a)
fromPg_Tup1 (FromPg {typename: typenameA, parser: parserA}) =
  FromPg
    { typename: "Tup1 of " <> typenameA
    , parser:
        contextualize "while parsing Tup1"
        $ \expr -> do
          subExprs <- parseComposite { open: "(", delim: ",", close: ")" } expr
          subExpr <-
            case subExprs of
              [subExpr] -> Right subExpr
              _ -> Left $ mkErr (Just expr) "`fromPg_Tup1` incorrect number of `subExprs`"
          val <- parserA subExpr
          pure (Tup val)
    }

fromPg_Tup2 :: forall a b. FromPg a -> FromPg b -> FromPg (Tup (a /\ b))
fromPg_Tup2
  (FromPg {typename: typenameA, parser: parserA})
  (FromPg {typename: typenameB, parser: parserB}) =
  FromPg
    { typename: "Tup2 of (" <> typenameA <> " /\ " <> typenameB <> ")"
    , parser:
        contextualize "while parsing Tup2"
        $ \expr -> do
          subExprs <- parseComposite { open: "(", delim: ",", close: ")" } expr
          (subExpr1 /\ subExpr2) <-
            case subExprs of
              [subExpr1, subExpr2] -> Right (subExpr1 /\ subExpr2)
              _ -> Left $ mkErr (Just expr) "`fromPg_Tup2` incorrect number of `subExprs`"
          val1 <- parserA subExpr1
          val2 <- parserB subExpr2
          pure (Tup (val1 /\ val2))
    }

fromPg_Tup4 :: forall a b c d. FromPg a ->  FromPg b -> FromPg c -> FromPg d -> FromPg (Tup (a /\ b /\ c /\ d))
fromPg_Tup4
  (FromPg {typename: typenameA, parser: parserA})
  (FromPg {typename: typenameB, parser: parserB})
  (FromPg {typename: typenameC, parser: parserC})
  (FromPg {typename: typenameD, parser: parserD}) =
  FromPg
    { typename: "Tup4 of (" <> typenameA <> " /\ " <> typenameB <> " /\ " <> typenameC <> " /\ " <> typenameD <> ")"
    , parser:
        contextualize "while parsing Tup4"
        $ \expr -> do
          subExprs <- parseComposite { open: "(", delim: ",", close: ")" } expr
          (subExpr1 /\ subExpr2 /\ subExpr3 /\ subExpr4) <-
            case subExprs of
              [subExpr1, subExpr2, subExpr3, subExpr4] -> Right (subExpr1 /\ subExpr2 /\ subExpr3 /\ subExpr4)
              _ -> Left $ mkErr (Just expr) "`fromPg_Tup4` incorrect number of `subExprs`"
          val1 <- parserA subExpr1
          val2 <- parserB subExpr2
          val3 <- parserC subExpr3
          val4 <- parserD subExpr4
          pure (Tup (val1 /\ val2 /\ val3 /\ val4))
    }

-- | Parse an integral-format number (e.g., INT, BIGINT)
fromPg_Int :: FromPg Int
fromPg_Int =
  FromPg
    { typename: "integral number"
    , parser:
        contextualize "while parsing Int (SMALLINT, INT, BIGINT)"
        $ \expr -> Int.fromString (un PgExpr expr) # maybe (Left $ mkErr (Just expr) "Bad format") pure
    }

-- | Parse a decimal-format number (e.g., INT, FLOATING, DOUBLE PRECISION)
fromPg_Number :: FromPg Number
fromPg_Number =
  FromPg
    { typename: "decimal number"
    , parser:
        contextualize "while parsing Number (REAL, FIXED)"
        $ \expr -> Number.fromString (un PgExpr expr) # maybe (Left $ mkErr (Just expr) "Bad format") pure
    }

-- | Parse an `a` or NULL
fromPg_Maybe :: forall a. FromPg a -> FromPg (Maybe a)
fromPg_Maybe (FromPg {typename: typenameA, parser: parserA}) =
  FromPg
    { typename: "nullable " <> typenameA
    , parser: case _ of
        PgExpr "" -> pure Nothing
        expr -> Just <$> parserA expr
    }

-- | Parse as text (e.g., TEXT)
fromPg_String :: FromPg String
fromPg_String =
  FromPg { typename: "string", parser: un PgExpr >>> pure }

-- | Parse an array
fromPg_Array :: forall a. FromPg a -> FromPg (Array a)
fromPg_Array (FromPg {typename: typenameA, parser: parserA}) =
  FromPg
    { typename: "array of " <> typenameA
    , parser:
        contextualize "while parsing Array"
        $ \expr -> do
          subExprs <- parseComposite { open: "{", delim: ",", close: "}" } expr
          vals <- traverse parserA subExprs
          pure vals
    }

-- | Parse an array, then turn it into a set
fromPg_Set :: forall a. Ord a => FromPg a -> FromPg (Set a)
fromPg_Set (FromPg {typename: typenameA, parser: parserA}) =
  FromPg
    { typename: "set of " <> typenameA
    , parser:
        contextualize "while parsing Set"
        $ \expr -> do
          subExprs <- parseComposite { open: "{", delim: ",", close: "}" } expr
          vals <- traverse parserA subExprs
          pure $ Set.fromFoldable vals
    }
