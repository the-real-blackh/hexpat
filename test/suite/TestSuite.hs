module Main where

import qualified Text.XML.Expat.UnitTests
import qualified Text.XML.Expat.Cursor.Tests
import qualified Text.XML.Expat.Proc.Tests

import Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain tests
  where tests = [ testGroup "unit tests"
                            Text.XML.Expat.UnitTests.tests
                , testGroup "Text.XML.Expat.Proc"
                            Text.XML.Expat.Proc.Tests.tests
                , testGroup "Text.XML.Expat.Cursor"
                            Text.XML.Expat.Cursor.Tests.tests
                ]
