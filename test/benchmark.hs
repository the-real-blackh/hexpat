import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Text.XML.Expat.Qualified
import Text.XML.Expat.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Internal as I
import qualified Data.Text as T
import Data.Char
import Data.Maybe
import Control.Exception as E
import Control.Monad
import Control.Parallel.Strategies
import Test.HUnit hiding (Node)
import System.IO
import Foreign.ForeignPtr
import Foreign.Ptr
import Microbench
import Text.XML.HaXml as HaXml

instance NFData B.ByteString where
    rnf bs = ()

instance NFData T.Text where
    rnf bs = ()

instance NFData Document where
    rnf (Document _ _ root _) = rnf root

instance NFData Element where
    rnf (Elem name attrs content) = rnf (name, attrs, content)

instance NFData AttValue where
    rnf (AttValue val) = rnf val

instance NFData Reference where
    rnf ref = ()

instance NFData Content where
    rnf content = ()

parseOnly :: B.ByteString -> String -> IO ()
parseOnly xml _ = do
    parser <- newParser Nothing
    parse parser xml
    return ()

myParseTree enc xml = parseTree enc (L.fromChunks [xml])

fromRight :: Either XMLParseError a -> a
fromRight x = case x of
    Right right -> right
    Left err -> error (show err)

tests :: [(String, B.ByteString -> String -> IO ())]
tests = [
    ("HaXml", \_ xml -> rnf (HaXml.xmlParse "input" xml) `seq` return ()),
    ("low-level parse only, no tree", parseOnly),
    ("lazy parseTree string", \xml _ -> rnf (fst $ myParseTree Nothing xml :: UNode String) `seq` return ()),
    ("lazy parseTree byteString", \xml _ -> rnf (fst $ myParseTree Nothing xml :: UNode B.ByteString) `seq` return ()),
    ("lazy parseTree text", \xml _ -> rnf (fst $ myParseTree Nothing xml :: UNode T.Text) `seq` return ()),
    ("lazy parseTree qualifiedString", \xml _ -> rnf (toQualified $ fst $ myParseTree Nothing xml :: QNode String) `seq` return ()),
    ("lazy parseTree qualifiedByteString", \xml _ -> rnf (toQualified $ fst $ myParseTree Nothing xml :: QNode B.ByteString) `seq` return ()),
    ("lazy parseTree qualifiedText", \xml _ -> (toQualified $ fst $ myParseTree Nothing xml :: QNode T.Text) `seq` return ()),
    ("strict parseTree' string", \xml _ -> rnf (fromRight $ parseTree' Nothing xml :: UNode String) `seq` return ()),
    ("strict parseTree' byteString", \xml _ -> rnf (fromRight $ parseTree' Nothing xml :: UNode B.ByteString) `seq` return ()),
    ("strict parseTree' text", \xml _ -> rnf (fromRight $ parseTree' Nothing xml :: UNode T.Text) `seq` return ()),
    ("strict parseTree' qualifiedString", \xml _ -> rnf (toQualified $ fromRight $ parseTree' Nothing xml :: QNode String) `seq` return ()),
    ("strict parseTree' qualifiedByteString", \xml _ -> rnf (toQualified $ fromRight $ parseTree' Nothing xml :: QNode B.ByteString) `seq` return ()),
    ("strict parseTree' qualifiedText", \xml _ -> rnf (toQualified $ fromRight $ parseTree' Nothing xml :: QNode T.Text) `seq` return ())
  ]

myCopy :: B.ByteString -> IO B.ByteString
myCopy (I.PS x s l) = I.create l $ \p -> withForeignPtr x $ \f ->
    I.memcpy p (f `plusPtr` s) (fromIntegral l)

main = do
    xml <- B.readFile "test.xml"
    forM_ tests $ \(a,b) -> microbench a $ do
        copy <- myCopy xml
        let xmlStr = map w2c $ B.unpack copy
        b copy xmlStr

