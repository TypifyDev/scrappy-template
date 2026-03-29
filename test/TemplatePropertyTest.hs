{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : TemplatePropertyTest
-- Description : QuickCheck property tests for Scrappy.Template
-- Copyright   : (c) Galen Sprout, 2024
-- License     : BSD-3-Clause
-- Maintainer  : galen.sprout@gmail.com
--
-- Property-based tests verifying roundtrip parsing, substitution correctness,
-- no re-expansion, delimiter isolation, and identity laws for the template engine.

-- | @since 0.1.0.0
module TemplatePropertyTest (spec) where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Gen, (==>), forAll, property, choose, elements
  , oneof, suchThat, vectorOf, counterexample )

import Text.Parsec (parse, eof, char)

import Scrappy.Template
  ( manyTill_, validName, Name
  , templateParser, templateParserWith
  , unTemplate, unTemplateWith )

import qualified Data.Map as Map
import Data.Either (isLeft)
import Data.List (isInfixOf)


-- =========================================================================
-- Generators
-- =========================================================================

-- | Generate a valid template name: starts with letter\/underscore\/hyphen,
-- followed by alphanumeric\/underscore\/hyphen.
--
-- @since 0.1.0.0
genValidName :: Gen Name
genValidName = do
  first_ <- oneof
    [ elements (['a'..'z'] ++ ['A'..'Z'])
    , pure '_'
    , pure '-'
    ]
  n <- choose (0, 20 :: Int)
  rest <- vectorOf n $ oneof
    [ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
    , pure '_'
    , pure '-'
    ]
  pure (first_ : rest)

-- | Generate a name that starts with a digit (always invalid).
--
-- @since 0.1.0.0
genDigitStartName :: Gen String
genDigitStartName = do
  d <- elements ['0'..'9']
  n <- choose (0, 10 :: Int)
  rest <- vectorOf n $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_', '-'])
  pure (d : rest)

-- | Generate a pair of safe delimiters that contain no name-chars
-- (letters, digits, underscore, hyphen) to avoid ambiguity with validName.
--
-- @since 0.1.0.0
genSafeDelimiterPair :: Gen (String, String)
genSafeDelimiterPair = elements
  [ ("{{>>=", "}}")
  , ("{{::=", "}}")
  , ("{%", "%}")
  , ("<<", ">>")
  , ("[", "]")
  , ("<<<START>>>", "<<<END>>>")
  , ("((=", "))")
  , ("{|", "|}")
  ]

-- | Generate a non-colliding delimiter pair — one that does NOT start with @{{>>=@
-- so it won't accidentally match the default syntax.
--
-- @since 0.1.0.0
genNonDefaultDelimiterPair :: Gen (String, String)
genNonDefaultDelimiterPair = elements
  [ ("{{::=", "}}")
  , ("{%", "%}")
  , ("<<", ">>")
  , ("[", "]")
  , ("((=", "))")
  , ("{|", "|}")
  ]

-- | Generate a plain text string guaranteed not to contain the default
-- template opening delimiter @{{>>=@.
--
-- @since 0.1.0.0
genPlainText :: Gen String
genPlainText = do
  n <- choose (0, 50 :: Int)
  vectorOf n (elements safeChars)
    `suchThat` (\s -> not ("{{>>=" `isInfixOf` s))
  where
    safeChars :: [Char]
    safeChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,;:!?+-*/"

-- | Generate a plain text string guaranteed not to contain a given
-- delimiter pair as substrings.
--
-- @since 0.1.0.0
genPlainTextFor :: String -> String -> Gen String
genPlainTextFor open close = do
  n <- choose (0, 50 :: Int)
  vectorOf n (elements safeChars)
    `suchThat` (\s -> not (open `isInfixOf` s) && not (close `isInfixOf` s))
  where
    safeChars :: [Char]
    safeChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,;:!?+-*/"


-- =========================================================================
-- Helpers
-- =========================================================================

-- | Force a string to its full spine and return it.
--
-- @since 0.1.0.0
forceString :: String -> String
forceString [] = []
forceString (x:xs) = x `seq` forceString xs `seq` (x:xs)
{-# INLINE forceString #-}


-- =========================================================================
-- Properties
-- =========================================================================

-- | @since 0.1.0.0
spec :: Spec
spec = do

  -- =======================================================================
  -- validName
  -- =======================================================================
  describe "Property: validName" $ do

    prop "roundtrip: generated valid names parse back to themselves" $
      forAll genValidName $ \name ->
        let !s = "" :: String
        in  parse (validName <* eof) s name == Right name

    prop "rejects digit-start names" $
      forAll genDigitStartName $ \name ->
        let !name' = forceString name
            !s = "" :: String
        in  isLeft (parse (validName <* eof) s name')

    prop "rejects empty string" $
      property $
        let !s = "" :: String
        in  isLeft (parse (validName <* eof) s "")

    prop "first char of generated name is letter, underscore, or hyphen" $
      forAll genValidName $ \name ->
        let !name' = forceString name
            validFirst = ['a'..'z'] ++ ['A'..'Z'] ++ ['_', '-']
        in  case name' of
              (c:_) -> c `elem` validFirst
              []    -> error "genValidName produced empty string"

    prop "rest chars of generated name are alphanumeric, underscore, or hyphen" $
      forAll genValidName $ \name ->
        let !name' = forceString name
            validRest = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_', '-']
        in  case name' of
              (_:rest) -> all (`elem` validRest) rest
              []       -> error "genValidName produced empty string"

  -- =======================================================================
  -- templateParser
  -- =======================================================================
  describe "Property: templateParser" $ do

    prop "roundtrip: wrapping a valid name in {{>>= ... }} parses to the name" $
      forAll genValidName $ \name ->
        let !s = "" :: String
        in  parse (templateParser <* eof) s ("{{>>=" ++ name ++ "}}") == Right name

    prop "roundtrip with arbitrary safe delimiters" $
      forAll genSafeDelimiterPair $ \(open, close) ->
      forAll genValidName $ \name ->
        let !s = "" :: String
            !close' = forceString close
        in  parse (templateParserWith open close' <* eof) s (open ++ name ++ close') == Right name

    prop "mismatched delimiters cause parse failure" $
      forAll genValidName $ \name ->
        let !name' = forceString name
            !s = "" :: String
            !close' = forceString "}}"
            !input = forceString ("{{>>=" ++ name' ++ "}}")
        in  isLeft (parse (templateParserWith "{{::=" close' <* eof) s input)

    prop "empty name between delimiters fails" $
      forAll genSafeDelimiterPair $ \(open, close) ->
        let !close' = forceString close
            !s = "" :: String
            !input = forceString (open ++ close')
        in  isLeft (parse (templateParserWith open close' <* eof) s input)

    prop "digit-start name between delimiters fails" $
      forAll genSafeDelimiterPair $ \(open, close) ->
      forAll genDigitStartName $ \name ->
        let !name' = forceString name
            !close' = forceString close
            !s = "" :: String
            !input = forceString (open ++ name' ++ close')
        in  isLeft (parse (templateParserWith open close' <* eof) s input)

  -- =======================================================================
  -- unTemplate
  -- =======================================================================
  describe "Property: unTemplate" $ do

    prop "single key substitution" $
      forAll genValidName $ \k ->
      forAll genPlainText $ \v ->
        unTemplate (Map.singleton k v) ("{{>>=" ++ k ++ "}}") == v

    prop "multiple keys: concatenated placeholders produce concatenated values" $
      forAll genValidName $ \k1 ->
      forAll genValidName $ \k2 ->
      k1 /= k2 ==>
      forAll genPlainText $ \v1 ->
      forAll genPlainText $ \v2 ->
        let m = Map.fromList [(k1, v1), (k2, v2)]
            input = "{{>>=" ++ k1 ++ "}}{{>>=" ++ k2 ++ "}}"
        in  unTemplate m input == v1 ++ v2

    prop "no re-expansion: substitution value containing delimiters is literal" $
      forAll genValidName $ \k1 ->
      forAll genValidName $ \k2 ->
      k1 /= k2 ==>
        let dangerousValue = "{{>>=" ++ k2 ++ "}}"
        in  unTemplate (Map.singleton k1 dangerousValue) ("{{>>=" ++ k1 ++ "}}") == dangerousValue

    prop "plain text without placeholders passes through unchanged" $
      forAll genPlainText $ \s ->
        let !m = (Map.empty :: Map.Map String String)
        in  Map.size m `seq` unTemplate m s == s

    prop "empty input always yields empty output" $
      forAll genValidName $ \k ->
      forAll genPlainText $ \v ->
        let !k' = forceString k
            !v' = forceString v
            !m = Map.fromList [(k', v')]
        in  Map.size m `seq` unTemplate m "" == ""

    prop "surrounding text is preserved around placeholder" $
      forAll genValidName $ \k ->
      forAll genPlainText $ \v ->
      forAll genPlainText $ \prefix ->
      forAll genPlainText $ \suffix ->
        unTemplate (Map.singleton k v) (prefix ++ "{{>>=" ++ k ++ "}}" ++ suffix)
          == prefix ++ v ++ suffix

    prop "repeated same placeholder yields repeated value" $
      forAll genValidName $ \k ->
      forAll genPlainText $ \v ->
      forAll (choose (1, 10 :: Int)) $ \n ->
        let m = Map.singleton k v
            input = concat (replicate n ("{{>>=" ++ k ++ "}}"))
        in  unTemplate m input == concat (replicate n v)

  -- =======================================================================
  -- unTemplateWith (delimiter isolation)
  -- =======================================================================
  describe "Property: unTemplateWith" $ do

    prop "single key with custom delimiters" $
      forAll genSafeDelimiterPair $ \(open, close) ->
      forAll genValidName $ \k ->
      forAll (genPlainTextFor open close) $ \v ->
        unTemplateWith open close (Map.singleton k v) (open ++ k ++ close) == v

    prop "default placeholders ignored by non-default delimiters" $
      forAll genNonDefaultDelimiterPair $ \(open, close) ->
      forAll genValidName $ \k ->
        let !close' = forceString close
            !m = (Map.empty :: Map.Map String String)
            input = "{{>>=" ++ k ++ "}}"
        in  Map.size m `seq` unTemplateWith open close' m input == input

    prop "custom placeholders ignored by default unTemplate" $
      forAll genNonDefaultDelimiterPair $ \(open, close) ->
      forAll genValidName $ \k ->
        let input = open ++ k ++ close
            !m = (Map.empty :: Map.Map String String)
        in  not ("{{>>=" `isInfixOf` input) ==>
            Map.size m `seq` unTemplate m input == input

    prop "surrounding text preserved with custom delimiters" $
      forAll genSafeDelimiterPair $ \(open, close) ->
      forAll genValidName $ \k ->
      forAll (genPlainTextFor open close) $ \v ->
      forAll (genPlainTextFor open close) $ \prefix ->
      forAll (genPlainTextFor open close) $ \suffix ->
        unTemplateWith open close (Map.singleton k v)
          (prefix ++ open ++ k ++ close ++ suffix)
          == prefix ++ v ++ suffix

  -- =======================================================================
  -- manyTill_
  -- =======================================================================
  describe "Property: manyTill_" $ do

    prop "n copies of 'a' followed by 'b' yields (replicate n 'a', 'b')" $
      forAll (choose (0, 100 :: Int)) $ \n ->
        let input = replicate n 'a' ++ "b"
            !s = "" :: String
            result = parse (manyTill_ (char 'a') (char 'b')) s input
        in  result == Right (replicate n 'a', 'b')

  -- =======================================================================
  -- Cross-cutting
  -- =======================================================================
  describe "Property: cross-cutting" $ do

    prop "two-pass substitution: default then custom on same string" $
      forAll genValidName $ \kDefault ->
      forAll genValidName $ \kCustom ->
      forAll genPlainText $ \vDefault ->
      forAll genPlainText $ \vCustom ->
        let input = "{{>>=" ++ kDefault ++ "}} and {{::=" ++ kCustom ++ "}}"
            pass1 = unTemplate (Map.singleton kDefault vDefault) input
            pass2 = unTemplateWith "{{::=" "}}" (Map.singleton kCustom vCustom) pass1
            !msg1 = forceString ("pass1 = " ++ show pass1)
            !msg2 = forceString ("pass2 = " ++ show pass2)
        in  counterexample msg1 $
            counterexample msg2 $
            pass2 == vDefault ++ " and " ++ vCustom
