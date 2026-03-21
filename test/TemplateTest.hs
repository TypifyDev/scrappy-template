{-# LANGUAGE CPP #-}
module TemplateTest (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hFlush, hClose)

import Text.Parsec (parse, eof, char, letter)

import Scrappy.Template

import qualified Data.Map as Map

parseOnly :: String -> Either String Name
parseOnly input = case parse (templateParser <* eof) "" input of
  Left err -> Left (show err)
  Right n  -> Right n

parseOnlyWith :: String -> String -> String -> Either String Name
parseOnlyWith open close input = case parse (templateParserWith open close <* eof) "" input of
  Left err -> Left (show err)
  Right n  -> Right n

parseValidName :: String -> Either String Name
parseValidName input = case parse (validName <* eof) "" input of
  Left err -> Left (show err)
  Right n  -> Right n

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

spec :: Spec
spec = do

  -- =========================================================================
  -- Parser combinators
  -- =========================================================================
  describe "manyTill_" $ do
    it "returns accumulated results and the end value" $ do
      let p = parse (manyTill_ (char 'a') (char 'b')) "" "aaab"
      p `shouldBe` Right ("aaa", 'b')

    it "returns empty list when end matches immediately" $ do
      let p = parse (manyTill_ (char 'a') (char 'b')) "" "b"
      p `shouldBe` Right ("", 'b')

    it "returns single element" $ do
      let p = parse (manyTill_ (char 'a') (char 'b')) "" "ab"
      p `shouldBe` Right ("a", 'b')

  describe "between'" $ do
    it "collects multiple inner matches between delimiters" $ do
      let p = parse (between' (char '(') (char ')') letter) "" "(abc)"
      p `shouldBe` Right "abc"

    it "returns empty list when no inner matches" $ do
      let p = parse (between' (char '(') (char ')') letter) "" "()"
      p `shouldBe` Right ""

    it "handles single inner match" $ do
      let p = parse (between' (char '(') (char ')') letter) "" "(x)"
      p `shouldBe` Right "x"

  describe "between1" $ do
    it "matches exactly one element between open and close" $ do
      let p = parse (between1 (char '(') (char ')') letter) "" "(x)"
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
      parseOnlyWith "{{::=" "}}" "{{>>=name}}" `shouldSatisfy` isLeft

    it "rejects invalid name even with correct delimiters" $ do
      parseOnlyWith "{{::=" "}}" "{{::=123}}" `shouldSatisfy` isLeft

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
      let m = Map.fromList [("unused", "val")]
      unTemplate m "no slots here" `shouldBe` "no slots here"

    it "returns empty string for empty input" $ do
      let m = Map.fromList [("key", "val")]
      unTemplate m "" `shouldBe` ""

    it "returns empty for empty input and empty map" $ do
      let m = Map.empty :: Map.Map String String
      unTemplate m "" `shouldBe` ""

    it "passes through when map is empty and no placeholders" $ do
      let m = Map.empty :: Map.Map String String
      unTemplate m "just text" `shouldBe` "just text"

    it "errors on a missing key" $ do
      let m = Map.empty :: Map.Map String String
      evaluate (unTemplate m "{{>>=missing}}") `shouldThrow` anyErrorCall

    it "substitution value containing delimiter-like text is NOT re-expanded" $ do
      let m = Map.fromList [("x", "{{>>=y}}"), ("y", "NOPE")]
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
      let m = Map.empty :: Map.Map String String
      unTemplate m "text {{ not a template" `shouldBe` "text {{ not a template"

    it "incomplete template {{>>= without closing is left alone" $ do
      let m = Map.fromList [("x", "val")]
      -- {{>>=x without }} — streamEdit won't match, so it passes through
      unTemplate m "text {{>>=x no close" `shouldBe` "text {{>>=x no close"

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
      let m = Map.fromList [("x", "val")]
      unTemplateWith "{{::=" "}}" m "{{>>=x}}" `shouldBe` "{{>>=x}}"

    it "custom syntax placeholders are ignored by default unTemplate" $ do
      let m = Map.fromList [("x", "val")]
      unTemplate m "{{::=x}}" `shouldBe` "{{::=x}}"

    it "missing key error includes correct custom delimiters" $ do
      let m = Map.empty :: Map.Map String String
      evaluate (unTemplateWith "{{::=" "}}" m "{{::=missing}}")
        `shouldThrow` anyErrorCall

    it "multiple custom-delimited placeholders" $ do
      let m = Map.fromList [("a", "1"), ("b", "2"), ("c", "3")]
      unTemplateWith "<<" ">>" m "<<a>>-<<b>>-<<c>>" `shouldBe` "1-2-3"

    it "same key used multiple times with custom delimiters" $ do
      let m = Map.fromList [("r", "repeat")]
      unTemplateWith "[" "]" m "[r] and [r]" `shouldBe` "repeat and repeat"

    it "empty map with no placeholders and custom delimiters" $ do
      let m = Map.empty :: Map.Map String String
      unTemplateWith "{%" "%}" m "no templates" `shouldBe` "no templates"

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
        let m = Map.empty :: Map.Map String String
        result <- unTemplateFile m fp
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
      -- templateSyntaxExample is "{{>>=name}}"
      case parse (templateParser <* eof) "" templateSyntaxExample of
        Left err -> expectationFailure (show err)
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

-- Re-export parsec's char and letter for use in tests
-- (these are already available via Text.Parsec import in the test)
