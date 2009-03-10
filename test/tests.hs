import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Text.XML.Expat.Qualified
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Internal (c2w, w2c)
import Data.Char
import Data.Maybe
import Control.Exception as E
import Control.Monad
import Control.Parallel.Strategies
import Test.HUnit hiding (Node)
import System.IO

toByteStringL :: String -> BSL.ByteString
toByteStringL = BSL.pack . map c2w

fromByteStringL :: BSL.ByteString -> String
fromByteStringL = map w2c . BSL.unpack

toByteString :: String -> BS.ByteString
toByteString = BS.pack . map c2w

fromByteString :: BS.ByteString -> String
fromByteString = map w2c . BS.unpack

testDoc :: (Show tag, Show text) =>
           (Maybe Encoding -> bs -> Either XMLParseError (Node tag text))
        -> (Node tag text -> BSL.ByteString)
        -> (String -> bs)
        -> String
        -> Int
        -> String
        -> IO ()
testDoc parse fmt toBS descr0 idx xml = do
  let eTree = parse (Just UTF8) (toBS xml)
      descr = descr0++" #"++show idx
  case eTree of
      Right tree -> do
          let out = fromByteStringL $ fmt tree
          assertEqual descr xml out
      Left error -> do
          hPutStrLn stderr $ "parse failed: "++show error
          assertFailure descr

simpleDocs = [
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"++
    "<test xmlns:abc=\"http://blacksapphire.com/abc\"><abc:test1 type=\"expression\">Cat &amp; mouse</abc:test1><test2 type=\"communication\" language=\"Rhyming slang\">Dog &amp; bone</test2></test>",

    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"++
    "<second><test><test1 type=\"expression\">Cat &amp; mouse</test1><test2 type=\"communication\" language=\"Rhyming slang\">Dog &amp; bone</test2></test><test>Rose &amp; Crown</test></second>",

    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<test>Cat &amp; mouse</test>"
  ]

eitherify f mEnc bs = do
    case f mEnc bs of
        (_, Just err)  -> Left err
        (doc, Nothing) -> Right doc

test_error1 :: IO ()
test_error1 = do
    let eDoc = parseTree' stringFlavor Nothing (toByteString "<hello></goodbye>")
    assertEqual "error1" (Left $ XMLParseError "mismatched tag" 1 9) eDoc

test_error2 :: IO ()
test_error2 = do
    assertEqual "error2" (
            Element {eName = "hello", eAttrs = [], eChildren = []},
            Just (XMLParseError "mismatched tag" 1 9)
        ) $ parseTree stringFlavor Nothing
              (toByteStringL "<hello></goodbye>")

test_error3 :: IO ()
test_error3 =
    assertEqual "error3" (
            Element {eName = "open", eAttrs = [], eChildren = [
                Element {eName = "test1", eAttrs = [], eChildren = [Text "Hello"]},
                Element {eName = "hello", eAttrs = [], eChildren = []}
            ]},
            Just (XMLParseError "mismatched tag" 1 35)
        ) $ parseTree stringFlavor Nothing
              (toByteStringL "<open><test1>Hello</test1><hello></goodbye>")

test_error4 :: IO ()
test_error4 = do
    let eDoc = parseTree' stringFlavor Nothing (toByteString "!")
    assertEqual "error1" (Left $ XMLParseError "not well-formed (invalid token)" 1 0) eDoc

main = do
    testXML <- readFile "test.xml"
    -- Remove trailing newline
    let testXML' = reverse . dropWhile (== '\n') . reverse $ testXML
        docs = simpleDocs ++ [testXML']
        t (descr, parse, fmt) = do
            forM_ (zip [1..] docs) $ \(idx, doc) ->
                testDoc parse fmt toByteStringL descr idx doc
        t' (descr, parse, fmt) = do
            forM_ (zip [1..] docs) $ \(idx, doc) ->
                testDoc parse fmt toByteString descr idx doc
    runTestTT $ TestList [
        TestCase $ t' ("String", parseTree' stringFlavor, formatTree stringFlavor),
        TestCase $ t' ("ByteString", parseTree' byteStringFlavor, formatTree byteStringFlavor),
        TestCase $ t' ("Text", parseTree' textFlavor, formatTree textFlavor),
        TestCase $ t ("String/Lazy", eitherify $ parseTree stringFlavor, formatTree stringFlavor),
        TestCase $ t ("ByteString/Lazy", eitherify $ parseTree byteStringFlavor, formatTree byteStringFlavor),
        TestCase $ t ("Text/Lazy", eitherify $ parseTree textFlavor, formatTree textFlavor),
        TestCase $ t' ("String/Qualified", parseTree' qualifiedStringFlavor, formatTree qualifiedStringFlavor),
        TestCase $ t' ("ByteString/Qualified", parseTree' qualifiedByteStringFlavor, formatTree qualifiedByteStringFlavor),
        TestCase $ t' ("Text/Qualified", parseTree' qualifiedTextFlavor, formatTree qualifiedTextFlavor),
        TestCase $ t ("String/Qualified/Lazy", eitherify $ parseTree qualifiedStringFlavor, formatTree qualifiedStringFlavor),
        TestCase $ t ("ByteString/Qualified/Lazy", eitherify $ parseTree qualifiedByteStringFlavor, formatTree qualifiedByteStringFlavor),
        TestCase $ t ("Text/Qualified/Lazy", eitherify $ parseTree qualifiedTextFlavor, formatTree qualifiedTextFlavor),
        TestCase $ test_error1,
        TestCase $ test_error2,
        TestCase $ test_error3,
        TestCase $ test_error4
      ]

