import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Text.XML.Expat.Qualified
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

instance NFData B.ByteString where
    rnf bs = ()

instance NFData T.Text where
    rnf bs = ()

tests :: [(String, L.ByteString -> IO ())]
tests = [
    ("lazy parseTree stringFlavor", \doc -> rnf (parseTree stringFlavor Nothing doc) `seq` return ()),
    ("lazy parseTree byteStringFlavor", \doc -> rnf (parseTree byteStringFlavor Nothing doc) `seq` return ()),
    ("lazy parseTree textFlavor", \doc -> rnf (parseTree textFlavor Nothing doc) `seq` return ()),
    ("lazy parseTree qualifiedStringFlavor", \doc -> rnf (parseTree qualifiedStringFlavor Nothing doc) `seq` return ()),
    ("lazy parseTree qualifiedByteStringFlavor", \doc -> rnf (parseTree qualifiedByteStringFlavor Nothing doc) `seq` return ()),
    ("lazy parseTree qualifiedTextFlavor", \doc -> rnf (parseTree qualifiedTextFlavor Nothing doc) `seq` return ()),
    ("strict parseTree' stringFlavor", \doc -> rnf (parseTree' stringFlavor Nothing doc) `seq` return ()),
    ("strict parseTree' byteStringFlavor", \doc -> rnf (parseTree' byteStringFlavor Nothing doc) `seq` return ()),
    ("strict parseTree' textFlavor", \doc -> rnf (parseTree' textFlavor Nothing doc) `seq` return ()),
    ("strict parseTree' qualifiedStringFlavor", \doc -> rnf (parseTree' qualifiedStringFlavor Nothing doc) `seq` return ()),
    ("strict parseTree' qualifiedByteStringFlavor", \doc -> rnf (parseTree' qualifiedByteStringFlavor Nothing doc) `seq` return ()),
    ("strict parseTree' qualifiedTextFlavor", \doc -> rnf (parseTree' qualifiedTextFlavor Nothing doc) `seq` return ())
  ]

myCopy :: B.ByteString -> IO B.ByteString
myCopy (I.PS x s l) = I.create l $ \p -> withForeignPtr x $ \f ->
    I.memcpy p (f `plusPtr` s) (fromIntegral l)

myLCopy :: L.ByteString -> IO L.ByteString
myLCopy bs = do
    let cs = L.toChunks bs
    cs' <- mapM myCopy cs
    return $ L.fromChunks cs'

main = do
    doc0 <- B.readFile "test.xml"
    let doc = L.fromChunks [doc0]
    forM_ tests $ \(a,b) -> microbench a $ myLCopy doc >>= b

