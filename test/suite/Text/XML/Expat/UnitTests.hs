module Text.XML.Expat.UnitTests where

import Text.XML.Expat.Tree hiding (parse)
import qualified Text.XML.Expat.Tree as Tree
import Text.XML.Expat.IO hiding (parse)
import qualified Text.XML.Expat.IO as IO
import Text.XML.Expat.Cursor
import Text.XML.Expat.Format
import Text.XML.Expat.Qualified
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Foreign
import Foreign.C
import Data.ByteString.Internal (c2w, w2c)
import Data.Char
import Data.Maybe
import Data.IORef
import Control.Applicative
import Control.Exception as E
import Control.Monad
import Control.DeepSeq
import Test.HUnit hiding (Node)
import System.IO

import Test.Framework.Providers.HUnit (hUnitTestToTests)


toByteStringL :: String -> L.ByteString
toByteStringL = L.pack . map c2w

fromByteStringL :: L.ByteString -> String
fromByteStringL = map w2c . L.unpack

toByteString :: String -> B.ByteString
toByteString = B.pack . map c2w

fromByteString :: B.ByteString -> String
fromByteString = map w2c . B.unpack

testDoc :: (Show tag, Show text) =>
           (ParseOptions tag text
                -> bs
                -> Either XMLParseError (Node tag text))
        -> (Node tag text -> L.ByteString)
        -> (String -> bs)
        -> String
        -> Int
        -> String
        -> IO ()
testDoc parseFn fmt toBS descr0 idx xml = do
    let eTree = parseFn popts (toBS xml)
        descr = descr0++" #"++show idx
    case eTree of
        Right tree -> do
            let out = fromByteStringL $ fmt tree
            assertEqual descr xml out
        Left error -> do
            hPutStrLn stderr $ "parse failed: "++show error
            assertFailure descr
  where
    popts = defaultParseOptions { overrideEncoding = Just UTF8 }


eitherify f mEnc bs = do
    case f mEnc bs of
        (_, Just err)  -> Left err
        (doc, Nothing) -> Right doc

test_error1 :: IO ()
test_error1 = do
    let eDoc = Tree.parse' defaultParseOptions (toByteString "<hello></goodbye>") :: Either XMLParseError (UNode String)
    assertEqual "error1" (Left $ XMLParseError "mismatched tag" (XMLParseLocation 1 9 9 0)) eDoc

test_error2 :: IO ()
test_error2 = do
    assertEqual "error2" (
            Element {eName = "hello", eAttributes = [], eChildren = []},
            Just (XMLParseError "mismatched tag" (XMLParseLocation 1 9 9 0))
        ) (Tree.parse defaultParseOptions
              (toByteStringL "<hello></goodbye>") :: (UNode String, Maybe XMLParseError))

test_error3 :: IO ()
test_error3 =
    assertEqual "error3" (
            Element {eName = "open", eAttributes = [], eChildren = [
                Element {eName = "test1", eAttributes = [], eChildren = [Text "Hello"]},
                Element {eName = "hello", eAttributes = [], eChildren = []}
            ]},
            Just (XMLParseError "mismatched tag" (XMLParseLocation 1 35 35 0))
        ) $ Tree.parse defaultParseOptions
              (toByteStringL "<open><test1>Hello</test1><hello></goodbye>")

test_error4 :: IO ()
test_error4 = do
    let eDoc = Tree.parse' defaultParseOptions (toByteString "!") :: Either XMLParseError (UNode String)
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
    setStartElementHandler parser $ \_ cname cattrs -> do
        name <- peekCString cname
        ref <- modifyIORef ref $ \l -> ("start "++name):l
        return True
    setEndElementHandler parser $ \_ cname -> do
        name <- peekCString cname
        ref <- modifyIORef ref $ \l -> ("end "++name):l
        return True
    IO.parse parser lazy
    l <- reverse <$> readIORef ref
    assertEqual "parse"
        ["start open","start test1","end test1","start hello","end hello","end open"]
        l


test_entities1 = do
    assertEqual "parse error" merr Nothing
    assertEqual "entity substitution" (Text "foo") c
  where
    xml = "<root>&entity;</root>"

    popts = defaultParseOptions { entityDecoder = Just entityLookup }

    (tree,merr) = Tree.parse popts $ toByteStringL xml

    c = current $ fromJust $ firstChild $ fromTree tree

    entityLookup b = if b == "entity"
                       then Just "foo"
                       else Nothing

test_entities2 = do
    assertEqual "wrong answer" (Element "html" [] [Text "\228"], Nothing) pr
  where
    pr :: (UNode String, Maybe XMLParseError)
    pr = Tree.parse opt $ LC.pack "<html>&auml;</html>"
        where
        opt =  defaultParseOptions
               { entityDecoder = Just ed }
        ed "auml" = Just "\228"
        ed _      = Nothing

test_textContent = do
    let tree = Element "cheese" [("type", "edam")]
            [Text "You don't actually ",
             Element "sub" [] [Text "have any "],
             Text "cheese at all",
             Text ", do you?"]
    assertEqual "textContent" "You don't actually have any cheese at all, do you?" (textContent tree)

testXMLFile :: IO String
testXMLFile = do
    s <- map w2c . B.unpack <$> B.readFile "test.xml"
    -- Remove trailing newline
    return (reverse . dropWhile (== '\n') . reverse $ s)

test_indent = do
    let tests = [
                ("#1",
                 toByteString "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<test/>",
                 toByteString "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<test/>"),
                ("#2",
                 toByteString "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<test>With some text in it</test>",
                 toByteString "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<test>With some text in it</test>"),
                ("#3",
                 toByteString $ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"++
                     "<test><ignorance/><freedom/><war/></test>",
                 toByteString $ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"++
                     "<test>\n  <ignorance/>\n  <freedom/>\n  <war/>\n</test>"),
                ("#4",
                 toByteString $ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"++
                     "<test><ignorance>strength</ignorance><freedom>Slavery</freedom><war>Peace</war></test>",
                 toByteString $ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"++
                     "<test>\n  <ignorance>strength</ignorance>\n  <freedom>Slavery</freedom>\n  <war>Peace</war>\n</test>"),
                ("#5",
                 toByteString $ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"++
                     "<test>Extra here<ministries><mini name=\"minitrue\">Ministry of Truth</mini>In between"++
                     "<mini name=\"minilove\">Ministry of Love</mini>\n  And some more"++
                     "<mini name=\"miniplenty\">Ministry of Plenty</mini>"++
                     "<mini name=\"minipax\">Ministry of Peace<at-war-with>Eurasia</at-war-with></mini></ministries>"++
                     "<wisdom><ignorance>strength</ignorance></wisdom></test>",
                 toByteString $ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"++
                     "<test>Extra here\n  <ministries>\n    <mini name=\"minitrue\">Ministry of Truth</mini>In between"++
                     "\n    <mini name=\"minilove\">Ministry of Love</mini>And some more"++
                     "\n    <mini name=\"miniplenty\">Ministry of Plenty</mini>"++
                     "\n    <mini name=\"minipax\">Ministry of Peace\n      <at-war-with>Eurasia</at-war-with>\n    </mini>\n  </ministries>"++
                     "\n  <wisdom>\n    <ignorance>strength</ignorance>\n  </wisdom>\n</test>")
            ]
    forM_ tests $ \(name, inp, outSB) -> do
        let eree = Tree.parse' defaultParseOptions inp :: Either XMLParseError (UNode String)
        case eree of
            Left err -> assertFailure $ show err
            Right tree -> do
                let outIS = format' (indent 2 tree)
                assertEqual name outSB outIS

test_setAttribute :: IO ()
test_setAttribute = do
    assertEqual "#1" [("abc", "def")] $ getAttributes $
            setAttribute "abc" "def"
                (Element "test" [] [])
    assertEqual "#2" [("abc", "def")] $ getAttributes $
            setAttribute "abc" "def"
                (Element "test" [("abc", "xyzzy")] [])
    assertEqual "#2" [("abc", "def"), ("abc", "xyzzy")] $ getAttributes $
            setAttribute "abc" "def"
                (Element "test" [("abc", "zapf"), ("abc", "xyzzy")] [])
    assertEqual "#3" [("zanzi", "zapf"), ("bar", "xyzzy"), ("abc", "def")] $ getAttributes $
            setAttribute "abc" "def"
                (Element "test" [("zanzi", "zapf"), ("bar", "xyzzy")] [])
    assertEqual "#4" [("zanzi", "zapf"), ("bar", "xyzzy")] $ getAttributes $
            deleteAttribute "abc"
                (Element "test" [("zanzi", "zapf"), ("bar", "xyzzy"), ("abc", "def")] [])
    assertEqual "#5" [("zanzi", "zapf"), ("abc", "def")] $ getAttributes $
            deleteAttribute "bar"
                (Element "test" [("zanzi", "zapf"), ("bar", "xyzzy"), ("abc", "def")] [])
    assertEqual "#6" [("zanzi", "zapf"), ("bar", "xyzzy"), ("abc", "def")] $ getAttributes $
            deleteAttribute "bumpf"
                (Element "test" [("zanzi", "zapf"), ("bar", "xyzzy"), ("abc", "def")] [])

simpleDocs = [
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"++
    "<test xmlns:abc=\"http://blacksapphire.com/abc\"><abc:test1 type=\"expression\">Cat &amp; mouse</abc:test1><test2 type=\"communication\" language=\"Rhyming slang\">Dog &amp; bone</test2></test>",

    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"++
    "<second><test><test1 type=\"expression\">Cat &amp; mouse</test1><test2 type=\"communication\" language=\"Rhyming slang\">Dog &amp; bone</test2></test><test>Rose &amp; Crown</test></second>",

    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<test>Cat &amp; mouse</test>"
  ]

data ParseFormatTest = ParseFormatTest {
    }

tests = hUnitTestToTests $
    TestList [
        t' ("String",
            Tree.parse' :: ParseOptions String String
                        -> B.ByteString
                        -> Either XMLParseError (Node String String),
            format),
        t' ("ByteString",
            Tree.parse' :: ParseOptions B.ByteString B.ByteString
                        -> B.ByteString
                        -> Either XMLParseError (Node B.ByteString B.ByteString),
            format),
        t' ("Text",
            Tree.parse' :: ParseOptions T.Text T.Text
                        -> B.ByteString
                        -> Either XMLParseError (Node T.Text T.Text),
            format),
        t ("String/Lazy",
            eitherify $ Tree.parse :: ParseOptions String String
                                   -> L.ByteString
                                   -> Either XMLParseError (Node String String),
            format),
        t ("ByteString/Lazy",
            eitherify $ Tree.parse :: ParseOptions B.ByteString B.ByteString
                                   -> L.ByteString
                                   -> Either XMLParseError (Node B.ByteString B.ByteString),
            format),
        t ("Text/Lazy",
            eitherify $ Tree.parse :: ParseOptions T.Text T.Text
                                   -> L.ByteString
                                   -> Either XMLParseError (Node T.Text T.Text),
            format),
        TestLabel "error1" $ TestCase $ test_error1,
        TestLabel "error2" $ TestCase $ test_error2,
        TestLabel "error3" $ TestCase $ test_error3,
        TestLabel "error4" $ TestCase $ test_error4,
        TestLabel "parse" $ TestCase $ test_parse,
        TestLabel "entities1" $ TestCase $ test_entities1,
        TestLabel "entities2" $ TestCase $ test_entities2,
        TestLabel "textContent" $ TestCase $ test_textContent,
        TestLabel "indent" $ TestCase $ test_indent,
        TestLabel "setAttribute" $ TestCase $ test_setAttribute
      ]

  where
    t (descr, parse, fmt) = TestLabel descr $ TestCase $ do
        f <- testXMLFile
        let docs = f:simpleDocs
        forM_ (zip [1..] docs) $ \(idx, doc) ->
            testDoc parse fmt toByteStringL descr idx doc

    t' (descr, parse, fmt) = TestLabel descr $ TestCase $ do
        f <- testXMLFile
        let docs = f:simpleDocs
        forM_ (zip [1..] docs) $ \(idx, doc) ->
            testDoc parse fmt toByteString descr idx doc
