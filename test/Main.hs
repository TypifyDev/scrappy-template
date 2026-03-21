module Main where

import Test.Hspec
import qualified TemplateTest

main :: IO ()
main = hspec TemplateTest.spec
