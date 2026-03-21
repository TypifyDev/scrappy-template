{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Scrappy.Template
  ( -- * Parser combinators
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

import Scrappy.Find

import Text.Parsec
#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import qualified Data.Map as Map


-- A particular use of this library-module is like a cheap FFI
  -- it is also for this reason that the errors are meant to not be recoverable - an error in a
  -- non-predetermined language is just too wide a case to consider (imo)

-- This is a module for templating files using Scrappy, like how Flask uses templates


-- TODO: move manyTill_ , between', between1 into Scrappy.ParsecExtra

manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go


between' :: Stream s m Char => ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m [a]
between' open close inside = do
  _ <- open
  (x, _) <- manyTill_ inside close
  pure x


between1 :: Stream s m Char => ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m a
between1 open end match = open *> match <* end


-- | Parser for valid template names: starts with letter, underscore, or hyphen;
-- followed by alphanumeric, underscore, or hyphen.
validName :: Stream s m Char => ParsecT s u m Name
validName = do
  first_ <- letter <|> char '_' <|> char '-'
  rest <- many $ alphaNum <|> char '_' <|> char '-'
  pure $ first_ : rest


type Name = String

-- | Template parser parameterized by open\/close delimiters.
--
-- @templateParserWith \"{{>>=\" \"}}\"@ is equivalent to 'templateParser'.
templateParserWith :: Stream s m Char => String -> String -> ParsecT s u m Name
templateParserWith open close =
  between1 (string open) (string close) validName

-- | Default template parser using @{{>>=name}}@ syntax.
templateParser :: Stream s m Char => ParsecT s u m Name
templateParser = templateParserWith "{{>>=" "}}"

templateSyntaxExample :: String
templateSyntaxExample = "{{>>=name}}"


-- TODO: use template haskell and staticWhich to make this compile time so that it fails
-- if an undefined name does not exist in the Map and can also give warnings when some are not used

-- | Replace all template occurrences in the input using the map,
-- parameterized by open\/close delimiters.
--
-- Calls 'error' if a placeholder references a name not present in the map.
unTemplateWith :: String -> String -> Map.Map String String -> String -> String
unTemplateWith open close tmplMap input = streamEdit (templateParserWith open close) mapLookup input
  where
    mapLookup :: Name -> String
    mapLookup n = case Map.lookup n tmplMap of
      Just v -> v
      Nothing -> error $ "name " <> open <> n <> close <> " not defined"

-- | Replace all @{{>>=name}}@ occurrences in the input using the map.
--
-- Calls 'error' if a placeholder references a name not present in the map.
unTemplate :: Map.Map String String -> String -> String
unTemplate = unTemplateWith "{{>>=" "}}"

-- | Like 'unTemplateWith' but reads from a file. Parameterized by delimiters.
-- TOD: make this catchable as an IOError
unTemplateFileWith :: String -> String -> Map.Map String String -> FilePath -> IO String
unTemplateFileWith open close tmplMap fp = do
  s <- readFile fp
  pure $ unTemplateWith open close tmplMap s

-- | Like 'unTemplate' but reads from a file.
-- TOD: make this catchable as an IOError
unTemplateFile :: Map.Map String String -> FilePath -> IO String
unTemplateFile = unTemplateFileWith "{{>>=" "}}"
