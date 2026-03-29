{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Scrappy.Template
-- Description : Template substitution engine with parameterized delimiters
-- Copyright   : (c) Galen Sprout, 2024
-- License     : BSD-3-Clause
-- Maintainer  : galen.sprout@gmail.com
--
-- Template substitution engine built on Parsec. Supports parameterized
-- open\/close delimiters for embedding named placeholders in strings and files.
-- The default syntax uses @{{>>=name}}@ but custom delimiters (e.g. @{{::=name}}@,
-- @\{%name%\}@) are supported via the @*With@ variants.
module Scrappy.Template
  ( -- * Parser combinators
    -- | Low-level Parsec combinators used internally but exported for reuse.
    manyTill_
  , between'
  , between1
    -- * Template name parser
  , validName
  , Name
    -- * Parameterized template functions (custom delimiters)
  , templateParserWith
  , unTemplateWith
  , unTemplateFileWith
    -- * Default template functions ({{>>=name}} syntax)
  , templateParser
  , unTemplate
  , unTemplateFile
  , templateSyntaxExample
  ) where

import Scrappy.Find (streamEdit)

import Text.Parsec
  ( ParsecT, Stream, (<|>), string, char, letter, alphaNum, many )
#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import qualified Data.Map as Map


-- TODO: move manyTill_ , between', between1 into Scrappy.ParsecExtra

-- | Like 'Text.Parsec.manyTill' but returns both the accumulated results and
-- the end value as a pair.
--
-- @since 0.1.0.0
manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go


-- | Parse zero or more occurrences of @inside@ between @open@ and @close@,
-- returning the collected inner results.
--
-- @since 0.1.0.0
between' :: Stream s m Char => ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m [a]
between' open close inside = do
  _ <- open
  (x, _) <- manyTill_ inside close
  pure x


-- | Parse exactly one occurrence of @match@ between @open@ and @end@.
--
-- @since 0.1.0.0
between1 :: Stream s m Char => ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m a
between1 open end match = open *> match <* end


-- | Parser for valid template names: starts with letter, underscore, or hyphen;
-- followed by alphanumeric, underscore, or hyphen.
--
-- @since 0.1.0.0
validName :: Stream s m Char => ParsecT s u m Name
validName = do
  first_ <- letter <|> char '_' <|> char '-'
  rest <- many $ alphaNum <|> char '_' <|> char '-'
  pure $ first_ : rest


-- | A template placeholder name (e.g. @\"name\"@ in @{{>>=name}}@).
--
-- @since 0.1.0.0
type Name = String

-- | Template parser parameterized by open\/close delimiters.
--
-- @templateParserWith \"{{>>=\" \"}}\"@ is equivalent to 'templateParser'.
--
-- @since 0.1.0.0
templateParserWith :: Stream s m Char => String -> String -> ParsecT s u m Name
templateParserWith open close =
  between1 (string open) (string close) validName

-- | Default template parser using @{{>>=name}}@ syntax.
--
-- @since 0.1.0.0
templateParser :: Stream s m Char => ParsecT s u m Name
templateParser = templateParserWith "{{>>=" "}}"

-- | Example string showing the default template syntax: @\"{{>>=name}}\"@.
--
-- @since 0.1.0.0
templateSyntaxExample :: String
templateSyntaxExample = "{{>>=name}}"


-- TODO: use template haskell and staticWhich to make this compile time so that it fails
-- if an undefined name does not exist in the Map and can also give warnings when some are not used

-- | Replace all template occurrences in the input using the map,
-- parameterized by open\/close delimiters.
--
-- Calls 'error' if a placeholder references a name not present in the map.
--
-- @since 0.1.0.0
unTemplateWith :: String -> String -> Map.Map String String -> String -> String
unTemplateWith open close tmplMap input = streamEdit (templateParserWith open close) mapLookup input
  where
    mapLookup :: Name -> String
    mapLookup n = case Map.lookup n tmplMap of
      Just v -> v
      Nothing -> error $! "name " <> open <> n <> close <> " not defined"

-- | Replace all @{{>>=name}}@ occurrences in the input using the map.
--
-- Calls 'error' if a placeholder references a name not present in the map.
--
-- @since 0.1.0.0
unTemplate :: Map.Map String String -> String -> String
unTemplate = unTemplateWith "{{>>=" "}}"

-- | Like 'unTemplateWith' but reads from a file. Parameterized by delimiters.
--
-- TODO: make this catchable as an IOError
--
-- @since 0.1.0.0
unTemplateFileWith :: String -> String -> Map.Map String String -> FilePath -> IO String
unTemplateFileWith open close tmplMap fp = do
  s <- readFile fp
  pure $ unTemplateWith open close tmplMap s

-- | Like 'unTemplate' but reads from a file.
--
-- TODO: make this catchable as an IOError
--
-- @since 0.1.0.0
unTemplateFile :: Map.Map String String -> FilePath -> IO String
unTemplateFile = unTemplateFileWith "{{>>=" "}}"
