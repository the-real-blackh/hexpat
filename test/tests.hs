import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Text.XML.Expat.Qualified
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Internal (c2w, w2c)
import Data.Char
import Data.Maybe
import Control.Monad
import Test.HUnit hiding (Node)
import System.IO

toByteString :: String -> BSL.ByteString
toByteString = BSL.pack . map c2w

fromByteString :: BSL.ByteString -> String
fromByteString = map w2c . BSL.unpack

testDoc :: (Show tag, Show text) =>
           (Maybe Encoding -> BSL.ByteString -> Either String (Node tag text))
        -> (Maybe Encoding -> Node tag text -> BSL.ByteString)
        -> String
        -> Int
        -> String
        -> IO ()
testDoc parse fmt descr0 idx xml = do
  let eTree = parse (Just UTF8) (toByteString xml)
      descr = descr0++" #"++show idx
  case eTree of
      Right tree -> do
          let out = fromByteString $ fmt (Just UTF8) tree
          assertEqual descr xml out
      Left error -> do
          hPutStrLn stderr $ "parse failed: "++error
          assertFailure descr

simpleDocs = [
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"++
    "<test xmlns:abc=\"http://blacksapphire.com/abc\"><abc:test1 type=\"expression\">Cat &amp; mouse</abc:test1><test2 type=\"communication\" language=\"Rhyming slang\">Dog &amp; bone</test2></test>",

    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"++
    "<second><test><test1 type=\"expression\">Cat &amp; mouse</test1><test2 type=\"communication\" language=\"Rhyming slang\">Dog &amp; bone</test2></test><test>Rose &amp; Crown</test></second>",

    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<test>Cat &amp; mouse</test>"
  ]

righten f mEnc bs = Right $ f mEnc bs

main = do
    testXML <- readFile "test.xml"
    -- Remove trailing newline
    let testXML' = reverse . dropWhile (== '\n') . reverse $ testXML
        docs = simpleDocs ++ [testXML']
        t (descr, parse, fmt) = do
            forM_ (zip [1..] docs) $ \(idx, doc) ->
                testDoc parse fmt descr idx doc
    runTestTT $ TestList [
        TestCase $ t ("String", parseTree' stringFlavor, formatTree stringFlavor),
        TestCase $ t ("ByteString", parseTree' byteStringFlavor, formatTree byteStringFlavor),
        TestCase $ t ("Text", parseTree' textFlavor, formatTree textFlavor),
        TestCase $ t ("String/Lazy", righten $ parseTree stringFlavor, formatTree stringFlavor),
        TestCase $ t ("ByteString/Lazy", righten $ parseTree byteStringFlavor, formatTree byteStringFlavor),
        TestCase $ t ("Text/Lazy", righten $ parseTree textFlavor, formatTree textFlavor),
        TestCase $ t ("String/Qualified", parseTree' qualifiedStringFlavor, formatTree qualifiedStringFlavor),
        TestCase $ t ("ByteString/Qualified", parseTree' qualifiedByteStringFlavor, formatTree qualifiedByteStringFlavor),
        TestCase $ t ("Text/Qualified", parseTree' qualifiedTextFlavor, formatTree qualifiedTextFlavor),
        TestCase $ t ("String/Qualified/Lazy", righten $ parseTree qualifiedStringFlavor, formatTree qualifiedStringFlavor),
        TestCase $ t ("ByteString/Qualified/Lazy", righten $ parseTree qualifiedByteStringFlavor, formatTree qualifiedByteStringFlavor),
        TestCase $ t ("Text/Qualified/Lazy", righten $ parseTree qualifiedTextFlavor, formatTree qualifiedTextFlavor)
      ]


