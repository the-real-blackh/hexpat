import Text.XML.Expat.Tree
import Text.XML.Expat.IO
import Text.XML.Expat.Format
import Text.XML.Expat.Qualified
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import CForeign
import Data.ByteString.Internal (c2w, w2c)
import Data.Char
import Data.Maybe
import Data.IORef
import Control.Applicative
import Control.Exception as E
import Control.Monad
import Control.Parallel.Strategies
import Test.HUnit hiding (Node)
import System.IO

toByteStringL :: String -> L.ByteString
toByteStringL = L.pack . map c2w

fromByteStringL :: L.ByteString -> String
fromByteStringL = map w2c . L.unpack

toByteString :: String -> B.ByteString
toByteString = B.pack . map c2w

fromByteString :: B.ByteString -> String
fromByteString = map w2c . B.unpack

testDoc :: (Show tag, Show text) =>
           (Maybe Encoding -> bs -> Either XMLParseError (Node tag text))
        -> (Node tag text -> L.ByteString)
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
    let eDoc = parseTree' Nothing (toByteString "<hello></goodbye>") :: Either XMLParseError (UNode String)
    assertEqual "error1" (Left $ XMLParseError "mismatched tag" (XMLParseLocation 1 9 9 0)) eDoc

test_error2 :: IO ()
test_error2 = do
    assertEqual "error2" (
            Element {eName = "hello", eAttrs = [], eChildren = []},
            Just (XMLParseError "mismatched tag" (XMLParseLocation 1 9 9 0))
        ) (parseTree Nothing
              (toByteStringL "<hello></goodbye>") :: (UNode String, Maybe XMLParseError))

test_error3 :: IO ()
test_error3 =
    assertEqual "error3" (
            Element {eName = "open", eAttrs = [], eChildren = [
                Element {eName = "test1", eAttrs = [], eChildren = [Text "Hello"]},
                Element {eName = "hello", eAttrs = [], eChildren = []}
            ]},
            Just (XMLParseError "mismatched tag" (XMLParseLocation 1 35 35 0))
        ) $ parseTree Nothing
              (toByteStringL "<open><test1>Hello</test1><hello></goodbye>")

test_error4 :: IO ()
test_error4 = do
    let eDoc = parseTree' Nothing (toByteString "!") :: Either XMLParseError (UNode String)
    assertEqual "error1" (Left $ XMLParseError "not well-formed (invalid token)"
        (XMLParseLocation 1 0 0 0)) eDoc

test_parse :: IO ()
test_parse = do
    ref <- newIORef []
    let lazy = L.fromChunks [
            toByteString "<open><tes",
            toByteString "t1>Hello</test",
            toByteString "1><hello></he",
            toByteString "llo></open>"]
    parser <- newParser Nothing
    setStartElementHandler parser $ \cname cattrs -> do
        name <- peekCString cname
        ref <- modifyIORef ref $ \l -> ("start "++name):l
        return True
    setEndElementHandler parser $ \cname -> do
        name <- peekCString cname
        ref <- modifyIORef ref $ \l -> ("end "++name):l
        return True
    parse parser lazy
    l <- reverse <$> readIORef ref
    assertEqual "parse"
        ["start open","start test1","end test1","start hello","end hello","end open"]
        l

test_textContent = do
    let tree = Element "cheese" [("type", "edam")]
            [Text "You don't actually ",
             Element "sub" [] [Text "have any "],
             Text "cheese at all",
             Text ", do you?"]
    assertEqual "textContent" "You don't actually have any cheese at all, do you?" (textContent tree)

main = do
    testXML <- map w2c . B.unpack <$> B.readFile "test.xml"
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
        TestCase $ t' ("String",
            parseTree' :: Maybe Encoding -> B.ByteString -> Either XMLParseError (Node String String),
            formatTree),
        TestCase $ t' ("ByteString",
            parseTree' :: Maybe Encoding -> B.ByteString -> Either XMLParseError (Node B.ByteString B.ByteString),
            formatTree),
        TestCase $ t' ("Text",
            parseTree' :: Maybe Encoding -> B.ByteString -> Either XMLParseError (Node T.Text T.Text),
            formatTree),
        TestCase $ t ("String/Lazy",
            eitherify $ parseTree :: Maybe Encoding -> L.ByteString -> Either XMLParseError (Node String String),
            formatTree),
        TestCase $ t ("ByteString/Lazy",
            eitherify $ parseTree :: Maybe Encoding -> L.ByteString -> Either XMLParseError (Node B.ByteString B.ByteString),
            formatTree),
        TestCase $ t ("Text/Lazy",
            eitherify $ parseTree :: Maybe Encoding -> L.ByteString -> Either XMLParseError (Node T.Text T.Text),
            formatTree),
        TestCase $ test_error1,
        TestCase $ test_error2,
        TestCase $ test_error3,
        TestCase $ test_error4,
        TestCase $ test_parse,
        TestCase $ test_textContent
      ]

