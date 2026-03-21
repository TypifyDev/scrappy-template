-- |
-- Module      : Main
-- Description : Test runner for scrappy-template
-- Copyright   : (c) Galen Sprout, 2024
-- License     : BSD-3-Clause
-- Maintainer  : galen.sprout@gmail.com
--
-- Entry point for the scrappy-template test suite.

-- | @since 0.1.0.0
module Main (main) where

import Test.Hspec (hspec)
import qualified TemplateTest
import qualified TemplatePropertyTest

-- | @since 0.1.0.0
main :: IO ()
main = hspec $ do
  TemplateTest.spec
  TemplatePropertyTest.spec
