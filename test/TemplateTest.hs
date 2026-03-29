{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : TemplateTest
-- Description : Tests for Scrappy.Template parser and substitution engine
-- Copyright   : (c) Galen Sprout, 2024
-- License     : BSD-3-Clause
-- Maintainer  : galen.sprout@gmail.com
--
-- Comprehensive test suite covering parser combinators, name validation,
-- template parsing with default and custom delimiters, string and file
-- substitution, and integration\/stress tests.

-- | @since 0.1.0.0
module TemplateTest (spec) where

import Test.Hspec
  ( Spec, describe, it, shouldBe, shouldSatisfy
  , expectationFailure )
import Control.Exception (evaluate, try, ErrorCall (..))
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hFlush, hClose)

import Text.Parsec (parse, eof, char, letter)

import Scrappy.Template
  ( manyTill_, between', between1, validName, Name
  , templateParserWith, unTemplateWith, unTemplateFileWith
  , templateParser, unTemplate, unTemplateFile, templateSyntaxExample )

import qualified Data.Map as Map
import Data.List (isInfixOf)

-- | @since 0.1.0.0
parseOnly :: String -> Either String Name
parseOnly input =
  let !s = "" :: String
  in  case parse (templateParser <* eof) s input of
        Left err -> let !msg = show err in Left msg
        Right n  -> Right n

-- | @since 0.1.0.0
parseOnlyWith :: String -> String -> String -> Either String Name
parseOnlyWith open close input =
  let !s = "" :: String
  in  case parse (templateParserWith open close <* eof) s input of
        Left err -> let !msg = show err in Left msg
        Right n  -> Right n

-- | @since 0.1.0.0
parseValidName :: String -> Either String Name
parseValidName input =
  let !s = "" :: String
  in  case parse (validName <* eof) s input of
        Left err -> let !msg = show err in Left msg
        Right n  -> Right n

-- | Check whether an Either is Left, handling both branches.
--
-- @since 0.1.0.0
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

-- | Force a string to its full spine and return it.
--
-- @since 0.1.0.0
forceStr :: String -> String
forceStr [] = []
forceStr (x:xs) = x `seq` forceStr xs `seq` (x:xs)
{-# INLINE forceStr #-}

-- | Full test suite for Scrappy.Template.
--
-- @since 0.1.0.0
spec :: Spec
spec = do

  -- =========================================================================
  -- Parser combinators
  -- =========================================================================
  describe "manyTill_" $ do
    it "returns accumulated results and the end value" $ do
      let !s = "" :: String
          p = parse (manyTill_ (char 'a') (char 'b')) s "aaab"
      p `shouldBe` Right ("aaa", 'b')

    it "returns empty list when end matches immediately" $ do
      let !s = "" :: String
          !pa = char 'a'
          p = parse (manyTill_ pa (char 'b')) s "b"
      p `shouldBe` Right ("", 'b')

    it "returns single element" $ do
      let !s = "" :: String
          p = parse (manyTill_ (char 'a') (char 'b')) s "ab"
      p `shouldBe` Right ("a", 'b')

  describe "between'" $ do
    it "collects multiple inner matches between delimiters" $ do
      let !s = "" :: String
          p = parse (between' (char '(') (char ')') letter) s "(abc)"
      p `shouldBe` Right "abc"

    it "returns empty list when no inner matches" $ do
      let !s = "" :: String
          !inner = letter
          p = parse (between' (char '(') (char ')') inner) s "()"
      p `shouldBe` Right ""

    it "handles single inner match" $ do
      let !s = "" :: String
          p = parse (between' (char '(') (char ')') letter) s "(x)"
      p `shouldBe` Right "x"

  describe "between1" $ do
    it "matches exactly one element between open and close" $ do
      let !s = "" :: String
          p = parse (between1 (char '(') (char ')') letter) s "(x)"
      p `shouldBe` Right 'x'

  -- =========================================================================
  -- validName
  -- =========================================================================
  describe "validName" $ do
    it "accepts a name starting with a letter" $ do
      parseValidName "hello" `shouldBe` Right "hello"

    it "accepts a name starting with underscore" $ do
      parseValidName "_foo" `shouldBe` Right "_foo"

    it "accepts a name starting with hyphen" $ do
      parseValidName "-bar" `shouldBe` Right "-bar"

    it "accepts alphanumeric continuation" $ do
      parseValidName "abc123" `shouldBe` Right "abc123"

    it "accepts mixed underscores and hyphens in rest" $ do
      parseValidName "a-b_c" `shouldBe` Right "a-b_c"

    it "accepts a single character name" $ do
      parseValidName "x" `shouldBe` Right "x"

    it "accepts a long name" $ do
      let name = "very-long_name-with-many123-parts456"
      parseValidName name `shouldBe` Right name

    it "rejects a name starting with a digit" $ do
      parseValidName "123abc" `shouldSatisfy` isLeft

    it "rejects an empty string" $ do
      parseValidName "" `shouldSatisfy` isLeft

    it "rejects special char @" $ do
      parseValidName "@foo" `shouldSatisfy` isLeft

    it "rejects special char !" $ do
      parseValidName "!bar" `shouldSatisfy` isLeft

    it "rejects a space" $ do
      parseValidName " name" `shouldSatisfy` isLeft

    it "rejects a dot-start" $ do
      parseValidName ".name" `shouldSatisfy` isLeft

    it "isLeft returns False for Right values" $ do
      isLeft (Right "x" :: Either String String) `shouldBe` False

  -- =========================================================================
  -- templateParser (default {{>>= syntax)
  -- =========================================================================
  describe "templateParser" $ do
    it "parses {{>>=name}}" $ do
      parseOnly "{{>>=name}}" `shouldBe` Right "name"

    it "parses {{>>=-hyphen-name}}" $ do
      parseOnly "{{>>=-hyphen-name}}" `shouldBe` Right "-hyphen-name"

    it "parses {{>>=_underscore}}" $ do
      parseOnly "{{>>=_underscore}}" `shouldBe` Right "_underscore"

    it "parses {{>>=mixed-Name_123}}" $ do
      parseOnly "{{>>=mixed-Name_123}}" `shouldBe` Right "mixed-Name_123"

    it "parses {{>>=x}} (single char name)" $ do
      parseOnly "{{>>=x}}" `shouldBe` Right "x"

    it "does NOT match {{::=name}} (wrong delimiter)" $ do
      parseOnly "{{::=name}}" `shouldSatisfy` isLeft

    it "does NOT match {{>>=}} (empty name)" $ do
      parseOnly "{{>>=}}" `shouldSatisfy` isLeft

    it "does NOT match {{>>=123start}} (digit-start)" $ do
      parseOnly "{{>>=123start}}" `shouldSatisfy` isLeft

    it "does NOT match bare text" $ do
      parseOnly "hello" `shouldSatisfy` isLeft

    it "does NOT match empty string" $ do
      parseOnly "" `shouldSatisfy` isLeft

  -- =========================================================================
  -- templateParserWith (custom delimiters)
  -- =========================================================================
  describe "templateParserWith" $ do
    it "parses {{::=name}} with Lamarckian delimiters" $ do
      parseOnlyWith "{{::=" "}}" "{{::=name}}" `shouldBe` Right "name"

    it "parses {%name%} with Jinja-style delimiters" $ do
      parseOnlyWith "{%" "%}" "{%name%}" `shouldBe` Right "name"

    it "parses <<name>> with angle delimiters" $ do
      parseOnlyWith "<<" ">>" "<<name>>" `shouldBe` Right "name"

    it "works with single-char delimiters" $ do
      parseOnlyWith "[" "]" "[myvar]" `shouldBe` Right "myvar"

    it "works with long/unusual delimiters" $ do
      parseOnlyWith "<<<START>>>" "<<<END>>>" "<<<START>>>hello<<<END>>>" `shouldBe` Right "hello"

    it "rejects wrong delimiter" $ do
      let !close' = forceStr "}}"
      parseOnlyWith "{{::=" close' "{{>>=name}}" `shouldSatisfy` isLeft

    it "rejects invalid name even with correct delimiters" $ do
      let !close' = forceStr "}}"
      parseOnlyWith "{{::=" close' "{{::=123}}" `shouldSatisfy` isLeft

  -- =========================================================================
  -- unTemplate (default syntax)
  -- =========================================================================
  describe "unTemplate" $ do
    it "substitutes a single placeholder" $ do
      let m = Map.fromList [("name", "world")]
      unTemplate m "hello {{>>=name}}!" `shouldBe` "hello world!"

    it "substitutes multiple distinct placeholders" $ do
      let m = Map.fromList [("a", "X"), ("b", "Y")]
      unTemplate m "{{>>=a}} and {{>>=b}}" `shouldBe` "X and Y"

    it "substitutes the same placeholder appearing twice" $ do
      let m = Map.fromList [("x", "Z")]
      unTemplate m "{{>>=x}} then {{>>=x}}" `shouldBe` "Z then Z"

    it "handles adjacent placeholders with no gap" $ do
      let m = Map.fromList [("a", "X"), ("b", "Y")]
      unTemplate m "{{>>=a}}{{>>=b}}" `shouldBe` "XY"

    it "handles placeholder at start of string" $ do
      let m = Map.fromList [("x", "START")]
      unTemplate m "{{>>=x}} rest" `shouldBe` "START rest"

    it "handles placeholder at end of string" $ do
      let m = Map.fromList [("x", "END")]
      unTemplate m "rest {{>>=x}}" `shouldBe` "rest END"

    it "handles placeholder as entire string" $ do
      let m = Map.fromList [("all", "everything")]
      unTemplate m "{{>>=all}}" `shouldBe` "everything"

    it "returns input unchanged when no placeholders exist" $ do
      let !m = Map.fromList [("unused", "val")]
      Map.size m `seq` unTemplate m "no slots here" `shouldBe` "no slots here"

    it "returns empty string for empty input" $ do
      let !m = Map.fromList [("key", "val")]
      Map.size m `seq` unTemplate m "" `shouldBe` ""

    it "returns empty for empty input and empty map" $ do
      let !m = (Map.empty :: Map.Map String String)
      Map.size m `seq` unTemplate m "" `shouldBe` ""

    it "passes through when map is empty and no placeholders" $ do
      let !m = (Map.empty :: Map.Map String String)
      Map.size m `seq` unTemplate m "just text" `shouldBe` "just text"

    it "errors on a missing key with correct message" $ do
      let !m = (Map.empty :: Map.Map String String)
      result <- Map.size m `seq` try (evaluate (unTemplate m "{{>>=missing}}"))
      case result of
        Left (ErrorCall msg) -> msg `shouldSatisfy` \s ->
          "missing" `isInfixOf` s && "not defined" `isInfixOf` s
        Right _ -> expectationFailure "expected error but got success"

    it "substitution value containing delimiter-like text is NOT re-expanded" $ do
      let !nope = forceStr "NOPE"
          m = Map.fromList [("x", "{{>>=y}}"), ("y", nope)]
      unTemplate m "{{>>=x}}" `shouldBe` "{{>>=y}}"

    it "substitution value can be empty string" $ do
      let m = Map.fromList [("x", "")]
      unTemplate m "before{{>>=x}}after" `shouldBe` "beforeafter"

    it "handles many placeholders" $ do
      let keys = map (\i -> ("k" ++ show i, "v" ++ show i)) [1 :: Int .. 50]
          m = Map.fromList keys
          input = concatMap (\(k, _) -> "{{>>=" ++ k ++ "}}") keys
          expected = concatMap snd keys
      unTemplate m input `shouldBe` expected

    it "handles placeholder surrounded by special chars" $ do
      let m = Map.fromList [("x", "val")]
      unTemplate m "!@#{{>>=x}}$%^" `shouldBe` "!@#val$%^"

    it "partial template syntax {{ is left alone" $ do
      let !m = (Map.empty :: Map.Map String String)
      Map.size m `seq` unTemplate m "text {{ not a template" `shouldBe` "text {{ not a template"

    it "incomplete template {{>>= without closing is left alone" $ do
      let !m = Map.fromList [("x", "val")]
      Map.size m `seq` unTemplate m "text {{>>=x no close" `shouldBe` "text {{>>=x no close"

    it "three distinct placeholders in a sentence" $ do
      let m = Map.fromList [("greeting", "Hello"), ("name", "Alice"), ("punct", "!")]
      unTemplate m "{{>>=greeting}}, {{>>=name}}{{>>=punct}}"
        `shouldBe` "Hello, Alice!"

    it "substitutes placeholder with multiline value" $ do
      let m = Map.fromList [("block", "line1\nline2\nline3")]
      unTemplate m "start\n{{>>=block}}\nend" `shouldBe` "start\nline1\nline2\nline3\nend"

  -- =========================================================================
  -- unTemplateWith (custom delimiters)
  -- =========================================================================
  describe "unTemplateWith" $ do
    it "works with Lamarckian {{::= delimiter" $ do
      let m = Map.fromList [("name", "world")]
      unTemplateWith "{{::=" "}}" m "hello {{::=name}}!" `shouldBe` "hello world!"

    it "works with arbitrary {%  %} delimiters" $ do
      let m = Map.fromList [("x", "42")]
      unTemplateWith "{%" "%}" m "value: {%x%}" `shouldBe` "value: 42"

    it "default syntax placeholders are ignored when using custom delimiters" $ do
      let !m = Map.fromList [("x", "val")]
          !close' = forceStr "}}"
      Map.size m `seq` unTemplateWith "{{::=" close' m "{{>>=x}}" `shouldBe` "{{>>=x}}"

    it "custom syntax placeholders are ignored by default unTemplate" $ do
      let !m = Map.fromList [("x", "val")]
      Map.size m `seq` unTemplate m "{{::=x}}" `shouldBe` "{{::=x}}"

    it "missing key error with custom delimiters includes correct message" $ do
      let !m = (Map.empty :: Map.Map String String)
      result <- Map.size m `seq` try (evaluate (unTemplateWith "{{::=" "}}" m "{{::=missing}}"))
      case result of
        Left (ErrorCall msg) -> msg `shouldSatisfy` \s ->
          "missing" `isInfixOf` s && "not defined" `isInfixOf` s
        Right _ -> expectationFailure "expected error but got success"

    it "multiple custom-delimited placeholders" $ do
      let m = Map.fromList [("a", "1"), ("b", "2"), ("c", "3")]
      unTemplateWith "<<" ">>" m "<<a>>-<<b>>-<<c>>" `shouldBe` "1-2-3"

    it "same key used multiple times with custom delimiters" $ do
      let m = Map.fromList [("r", "repeat")]
      unTemplateWith "[" "]" m "[r] and [r]" `shouldBe` "repeat and repeat"

    it "empty map with no placeholders and custom delimiters" $ do
      let !m = (Map.empty :: Map.Map String String)
          !close' = forceStr "%}"
      Map.size m `seq` unTemplateWith "{%" close' m "no templates" `shouldBe` "no templates"

  -- =========================================================================
  -- unTemplateFile / unTemplateFileWith
  -- =========================================================================
  describe "unTemplateFile" $ do
    it "reads a file and substitutes default-syntax templates" $ do
      withSystemTempFile "tmpl.txt" $ \fp h -> do
        hPutStr h "Hello {{>>=who}}!"
        hFlush h
        hClose h
        let m = Map.fromList [("who", "World")]
        result <- unTemplateFile m fp
        result `shouldBe` "Hello World!"

    it "handles file with no templates" $ do
      withSystemTempFile "tmpl.txt" $ \fp h -> do
        hPutStr h "plain text"
        hFlush h
        hClose h
        let !m = (Map.empty :: Map.Map String String)
        result <- Map.size m `seq` unTemplateFile m fp
        result `shouldBe` "plain text"

  describe "unTemplateFileWith" $ do
    it "reads a file and substitutes custom-syntax templates" $ do
      withSystemTempFile "tmpl.txt" $ \fp h -> do
        hPutStr h "val={{::=mykey}}"
        hFlush h
        hClose h
        let m = Map.fromList [("mykey", "42")]
        result <- unTemplateFileWith "{{::=" "}}" m fp
        result `shouldBe` "val=42"

    it "reads a file with multiple custom templates" $ do
      withSystemTempFile "tmpl.txt" $ \fp h -> do
        hPutStr h "{%a%} + {%b%} = {%c%}"
        hFlush h
        hClose h
        let m = Map.fromList [("a", "1"), ("b", "2"), ("c", "3")]
        result <- unTemplateFileWith "{%" "%}" m fp
        result `shouldBe` "1 + 2 = 3"

  -- =========================================================================
  -- Cross-cutting / integration
  -- =========================================================================
  describe "integration" $ do
    it "templateSyntaxExample parses with templateParser" $ do
      let !s = "" :: String
      case parse (templateParser <* eof) s templateSyntaxExample of
        Left err -> expectationFailure $! show err
        Right n  -> n `shouldBe` "name"

    it "unTemplate handles all valid name character types" $ do
      let m = Map.fromList
            [ ("abc", "letters")
            , ("_under", "underscore-start")
            , ("-hyph", "hyphen-start")
            , ("a1-b_2", "mixed")
            ]
      let input = "{{>>=abc}} {{>>=-hyph}} {{>>=_under}} {{>>=a1-b_2}}"
      unTemplate m input `shouldBe` "letters hyphen-start underscore-start mixed"

    it "stress: 100 placeholders in a large string" $ do
      let n = 100 :: Int
          keys = [("var" ++ show i, "val" ++ show i) | i <- [1..n]]
          m = Map.fromList keys
          input = unwords ["{{>>=var" ++ show i ++ "}}" | i <- [1..n]]
          expected = unwords ["val" ++ show i | i <- [1..n]]
      unTemplate m input `shouldBe` expected

    it "custom and default delimiters can coexist in the same string without interference" $ do
      let mDefault = Map.fromList [("d", "DEFAULT")]
          mCustom  = Map.fromList [("c", "CUSTOM")]
          input = "{{>>=d}} and {{::=c}}"
      -- First pass: substitute default
      let pass1 = unTemplate mDefault input
      pass1 `shouldBe` "DEFAULT and {{::=c}}"
      -- Second pass: substitute custom
      let pass2 = unTemplateWith "{{::=" "}}" mCustom pass1
      pass2 `shouldBe` "DEFAULT and CUSTOM"
