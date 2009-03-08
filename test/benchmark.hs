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

parseOnly :: L.ByteString -> String -> IO ()
parseOnly xml _ = do
    parser <- newParser Nothing
    parse parser xml
    return ()

tests :: [(String, L.ByteString -> String -> IO ())]
tests = [
    ("HaXml", \_ xml -> rnf (HaXml.xmlParse "input" xml) `seq` return ()),
    ("low-level parse only, no tree", parseOnly),
    ("lazy parseTree stringFlavor", \xml _ -> rnf (parseTree stringFlavor Nothing xml) `seq` return ()),
    ("lazy parseTree byteStringFlavor", \xml _ -> rnf (parseTree byteStringFlavor Nothing xml) `seq` return ()),
    ("lazy parseTree textFlavor", \xml _ -> rnf (parseTree textFlavor Nothing xml) `seq` return ()),
    ("lazy parseTree qualifiedStringFlavor", \xml _ -> rnf (parseTree qualifiedStringFlavor Nothing xml) `seq` return ()),
    ("lazy parseTree qualifiedByteStringFlavor", \xml _ -> rnf (parseTree qualifiedByteStringFlavor Nothing xml) `seq` return ()),
    ("lazy parseTree qualifiedTextFlavor", \xml _ -> rnf (parseTree qualifiedTextFlavor Nothing xml) `seq` return ()),
    ("strict parseTree' stringFlavor", \xml _ -> rnf (parseTree' stringFlavor Nothing xml) `seq` return ()),
    ("strict parseTree' byteStringFlavor", \xml _ -> rnf (parseTree' byteStringFlavor Nothing xml) `seq` return ()),
    ("strict parseTree' textFlavor", \xml _ -> rnf (parseTree' textFlavor Nothing xml) `seq` return ()),
    ("strict parseTree' qualifiedStringFlavor", \xml _ -> rnf (parseTree' qualifiedStringFlavor Nothing xml) `seq` return ()),
    ("strict parseTree' qualifiedByteStringFlavor", \xml _ -> rnf (parseTree' qualifiedByteStringFlavor Nothing xml) `seq` return ()),
    ("strict parseTree' qualifiedTextFlavor", \xml _ -> rnf (parseTree' qualifiedTextFlavor Nothing xml) `seq` return ())
  ]

myCopy :: B.ByteString -> IO B.ByteString
myCopy (I.PS x s l) = I.create l $ \p -> withForeignPtr x $ \f ->
    I.memcpy p (f `plusPtr` s) (fromIntegral l)

main = do
    xml <- B.readFile "test.xml"
    forM_ tests $ \(a,b) -> microbench a $ do
        copy <- myCopy xml
        let xmlStr = map w2c $ B.unpack copy
        b (L.fromChunks [copy]) xmlStr

